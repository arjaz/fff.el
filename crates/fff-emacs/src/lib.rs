//! Emacs module wrapping `fff-search`.
//!
//! One `FilePicker` per project root in a global table.
//! Elisp side stores the id.
//! Queries run synchronously against the index.

// `defun_prefix` adds fff--
#![allow(non_snake_case)]

use std::collections::HashMap;
use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::Mutex;
use std::time::Duration;

use emacs::{defun, Env, IntoLisp, Result, Value};
use fff_query_parser::{AiGrepConfig, DirSearchConfig, FuzzyQuery};
use fff_search::{
    DbHealthChecker, FFFMode, FFFQuery, FilePicker, FilePickerOptions, FileSearchConfig,
    FrecencyTracker, FuzzySearchOptions, GrepConfig, GrepMode, GrepSearchOptions, PaginationArgs,
    QueryTracker, SharedFilePicker, SharedFrecency, SharedQueryTracker,
};

emacs::plugin_is_GPL_compatible!();

struct Session {
    picker: SharedFilePicker,
    frecency: SharedFrecency,
    tracker: SharedQueryTracker,
    base: String,
    /// Why the frecency DB fell back to in-memory, if it did.
    frecency_error: Option<String>,
    /// Why the query-tracker DB fell back to in-memory, if it did.
    tracker_error: Option<String>,
}

static SESSIONS: std::sync::LazyLock<Mutex<HashMap<i64, Session>>> =
    std::sync::LazyLock::new(|| Mutex::new(HashMap::new()));
static NEXT_ID: AtomicI64 = AtomicI64::new(1);

fn lock_err() -> anyhow::Error {
    anyhow::anyhow!("fff: session lock poisoned")
}

fn with_session<T>(id: i64, f: impl FnOnce(&Session) -> Result<T>) -> Result<T> {
    let sessions = SESSIONS.lock().map_err(|_| lock_err())?;
    let s = sessions
        .get(&id)
        .ok_or_else(|| anyhow::anyhow!("fff: no session #{id}"))?;
    f(s)
}

fn with_picker<T>(id: i64, f: impl FnOnce(&FilePicker) -> Result<T>) -> Result<T> {
    with_session(id, |s| {
        let guard = s.picker.read().map_err(|e| anyhow::anyhow!("fff: {e}"))?;
        let picker = guard
            .as_ref()
            .ok_or_else(|| anyhow::anyhow!("fff: picker not ready"))?;
        f(picker)
    })
}

/// Create the DB dir for RAW, if one was configured.
/// Returns `Ok(None)` for nil/empty (in-memory requested),
/// `Ok(Some(dir))` when the dir exists, or `Err(msg)` when it
/// could not be created (caller falls back to in-memory).
fn prepare_db_dir(raw: Option<String>) -> std::result::Result<Option<String>, String> {
    let path = match raw {
        None => return Ok(None),
        Some(s) if s.trim().is_empty() => return Ok(None),
        Some(s) => s,
    };
    match std::fs::create_dir_all(&path) {
        Ok(()) => Ok(Some(path)),
        Err(e) => Err(format!("{path}: {e}")),
    }
}

/// Create or reuse a session for BASE-PATH. Returns the id.
/// Starts scan and watcher in the background.
/// FRECENCY-PATH/HISTORY-PATH are LMDB dirs (nil/empty means
/// in-memory). Unusable paths fall back to in-memory with the
/// reason kept for `db-status'; init itself never fails for them.
/// FOLLOW-SYMLINKS non-zero follows symlinks while indexing
/// (nil/omitted = off). ENABLE-HOME-DIR-SCANNING allows `$HOME'
/// as a root (omitted = allowed, mirrors nvim);
/// ENABLE-FS-ROOT-SCANNING allows `/' (nil/omitted = denied,
/// mirrors nvim). Elisp always passes explicit 1/0 for all three.
#[defun]
fn init(
    env: &Env,
    base_path: String,
    frecency_path: Option<String>,
    history_path: Option<String>,
    follow_symlinks: Option<i64>,
    enable_home_dir_scanning: Option<i64>,
    enable_fs_root_scanning: Option<i64>,
) -> Result<Value<'_>> {
    let canonical = std::path::PathBuf::from(&base_path)
        .canonicalize()
        .map(|p| p.to_string_lossy().into_owned())
        .unwrap_or(base_path);

    // Same root returns the live id.
    {
        let sessions = SESSIONS.lock().map_err(|_| lock_err())?;
        if let Some((id, _)) = sessions.iter().find(|(_, s)| s.base == canonical) {
            let id = *id;
            return id.into_lisp(env);
        }
    }

    let picker = SharedFilePicker::default();
    let frecency = SharedFrecency::default();
    let tracker = SharedQueryTracker::default();

    let frecency_error = match prepare_db_dir(frecency_path) {
        Ok(None) => None,
        Ok(Some(dir)) => match FrecencyTracker::open(&dir) {
            Ok(opened) => {
                if let Err(e) = frecency.init(opened) {
                    Some(format!("{dir}: {e}"))
                } else {
                    None
                }
            }
            Err(e) => Some(format!("{dir}: {e}")),
        },
        Err(msg) => Some(msg),
    };
    let tracker_error = match prepare_db_dir(history_path) {
        Ok(None) => None,
        Ok(Some(dir)) => match QueryTracker::open(&dir) {
            Ok(opened) => {
                if let Err(e) = tracker.init(opened) {
                    Some(format!("{dir}: {e}"))
                } else {
                    None
                }
            }
            Err(e) => Some(format!("{dir}: {e}")),
        },
        Err(msg) => Some(msg),
    };

    FilePicker::new_with_shared_state(
        picker.clone(),
        frecency.clone(),
        FilePickerOptions {
            base_path: canonical.clone(),
            enable_mmap_cache: true,
            enable_content_indexing: true,
            mode: FFFMode::Neovim,
            watch: true,
            cache_budget: None,
            follow_symlinks: resolve_follow_symlinks(follow_symlinks),
            enable_home_dir_scanning: resolve_enable_home_dir_scanning(enable_home_dir_scanning),
            enable_fs_root_scanning: resolve_enable_fs_root_scanning(enable_fs_root_scanning),
        },
    )
    .map_err(|e| anyhow::anyhow!("fff: init failed for {canonical}: {e}"))?;

    let id = NEXT_ID.fetch_add(1, Ordering::SeqCst);
    SESSIONS.lock().map_err(|_| lock_err())?.insert(
        id,
        Session {
            picker,
            frecency,
            tracker,
            base: canonical,
            frecency_error,
            tracker_error,
        },
    );
    id.into_lisp(env)
}

/// Wait for the initial scan, up to TIMEOUT-MS. Returns t/nil.
#[defun]
fn wait_for_scan(_env: &Env, id: i64, timeout_ms: i64) -> Result<bool> {
    with_session(id, |s| {
        Ok(s.picker
            .wait_for_scan(Duration::from_millis(timeout_ms.max(0) as u64)))
    })
}

/// Cap for unlimited queries. Big enough for export, small enough
/// to keep per-keystroke lists in the tens of MB.
const UNLIMITED_CAP: usize = 20_000;
const DEFAULT_LIMIT: usize = 100;
const DEFAULT_TIME_BUDGET_MS: u64 = 150;
/// Interactive default, mirrors `fff-max-matches-per-file'.
const DEFAULT_MAX_MATCHES_PER_FILE: usize = 100;
/// Interactive default, mirrors `fff-max-file-size' (10 MB).
const DEFAULT_MAX_FILE_SIZE: u64 = 10 * 1024 * 1024;

fn resolve_limit(raw: Option<i64>) -> usize {
    match raw {
        None => DEFAULT_LIMIT,
        Some(n) if n > 0 => (n as usize).min(UNLIMITED_CAP),
        // Elisp nil comes in as 0 via `fff--limit-arg'.
        Some(_) => UNLIMITED_CAP,
    }
}

fn resolve_time_budget(raw: Option<i64>) -> u64 {
    match raw {
        None => DEFAULT_TIME_BUDGET_MS,
        // Elisp nil/0 means no limit.
        Some(n) if n <= 0 => 0,
        Some(n) => n as u64,
    }
}

fn resolve_max_matches_per_file(raw: Option<i64>) -> usize {
    match raw {
        // Explicit 0 (Elisp nil/0) means unlimited, like nvim quickfix.
        Some(n) if n <= 0 => 0,
        Some(n) => (n as usize).min(UNLIMITED_CAP),
        // Omitted arg (legacy callers): interactive default.
        None => DEFAULT_MAX_MATCHES_PER_FILE,
    }
}

fn resolve_max_file_size(raw: Option<i64>) -> u64 {
    match raw {
        Some(n) if n > 0 => n as u64,
        // Elisp nil/0 and omitted args: default cap.
        _ => DEFAULT_MAX_FILE_SIZE,
    }
}

fn resolve_flag(raw: Option<i64>) -> bool {
    matches!(raw, Some(n) if n != 0)
}

fn resolve_flag_default_on(raw: Option<i64>) -> bool {
    match raw {
        None => true,
        Some(n) => n != 0,
    }
}

fn resolve_enforce_time_budget(raw: Option<i64>) -> bool {
    resolve_flag(raw)
}

/// Omitted/nil means on; explicit 0 means case-sensitive.
fn resolve_smart_case(raw: Option<i64>) -> bool {
    resolve_flag_default_on(raw)
}

fn resolve_filename_constraint(raw: Option<i64>) -> bool {
    resolve_flag(raw)
}

fn resolve_follow_symlinks(raw: Option<i64>) -> bool {
    resolve_flag(raw)
}

fn resolve_enable_home_dir_scanning(raw: Option<i64>) -> bool {
    resolve_flag_default_on(raw)
}

fn resolve_enable_fs_root_scanning(raw: Option<i64>) -> bool {
    resolve_flag(raw)
}

/// `fff-max-threads': nil/0/omitted means 0, i.e. all CPUs via
/// `available_parallelism()'. Positive values pass through.
fn resolve_max_threads(raw: Option<i64>) -> usize {
    match raw {
        Some(n) if n > 0 => n as usize,
        _ => 0,
    }
}

fn resolve_file_offset(raw: Option<i64>) -> usize {
    match raw {
        Some(n) if n > 0 => n as usize,
        // Omitted arg or Elisp 0: start over.
        _ => 0,
    }
}

/// Interactive defaults, mirror `fff-combo-boost-multiplier' /
/// `fff-combo-min-count'.
const DEFAULT_COMBO_BOOST: i32 = 100;
const DEFAULT_MIN_COMBO: u32 = 3;

fn resolve_combo_boost(raw: Option<i64>) -> i32 {
    match raw {
        Some(n) if n >= 0 => (n as i32).max(0),
        // Omitted arg (legacy callers): interactive default.
        _ => DEFAULT_COMBO_BOOST,
    }
}

fn resolve_min_combo(raw: Option<i64>) -> u32 {
    match raw {
        Some(n) if n >= 0 => n as u32,
        // Omitted arg (legacy callers): interactive default.
        _ => DEFAULT_MIN_COMBO,
    }
}

fn search_with_query<'a>(
    env: &'a Env,
    id: i64,
    query: &str,
    max_results: Option<i64>,
    combo_boost: Option<i64>,
    min_combo: Option<i64>,
    max_threads: Option<i64>,
) -> Result<Value<'a>> {
    let tracker: SharedQueryTracker = with_session(id, |s| Ok(s.tracker.clone()))?;
    with_picker(id, |picker| {
        let tracker_guard = tracker.read().map_err(|e| anyhow::anyhow!("fff: {e}"))?;
        let parsed = FFFQuery::parse(query, FileSearchConfig);
        let limit = resolve_limit(max_results);
        let results = picker.fuzzy_search(
            &parsed,
            tracker_guard.as_ref(),
            FuzzySearchOptions {
                max_threads: resolve_max_threads(max_threads),
                current_file: None,
                project_path: Some(picker.base_path()),
                combo_boost_score_multiplier: resolve_combo_boost(combo_boost),
                min_combo_count: resolve_min_combo(min_combo),
                pagination: PaginationArgs { offset: 0, limit },
            },
        );
        let vals: Vec<Value<'_>> = results
            .items
            .iter()
            .map(|item| item.relative_path(picker).into_lisp(env))
            .collect::<Result<_>>()?;
        env.list(&vals)
    })
}

/// Fuzzy file search. Returns repo-relative paths.
/// Keeps the historical 100/3 combo knobs; `search-ex' carries them.
/// MAX-THREADS nil/0/omitted means all CPUs.
#[defun]
fn search(
    env: &Env,
    id: i64,
    query: String,
    max_results: Option<i64>,
    max_threads: Option<i64>,
) -> Result<Value<'_>> {
    search_with_query(env, id, &query, max_results, None, None, max_threads)
}

/// Extended `search' with combo knobs: COMBO-BOOST multiplies the
/// query-history combo score, MIN-COMBO is the co-open count that
/// counts as a combo. Omitted means 100/3. MAX-THREADS nil/0/omitted
/// means all CPUs.
#[defun]
fn search_ex(
    env: &Env,
    id: i64,
    query: String,
    max_results: Option<i64>,
    combo_boost: Option<i64>,
    min_combo: Option<i64>,
    max_threads: Option<i64>,
) -> Result<Value<'_>> {
    search_with_query(
        env,
        id,
        &query,
        max_results,
        combo_boost,
        min_combo,
        max_threads,
    )
}

/// Directory-only fuzzy search. Returns repo-relative directory paths.
/// Parses QUERY with `DirSearchConfig' and calls
/// `fuzzy_search_directories'; combo knobs stay 0, like upstream.
/// MAX-THREADS nil/0/omitted means all CPUs.
#[defun]
fn search_directories(
    env: &Env,
    id: i64,
    query: String,
    max_results: Option<i64>,
    max_threads: Option<i64>,
) -> Result<Value<'_>> {
    with_picker(id, |picker| {
        let parsed = FFFQuery::parse(&query, DirSearchConfig);
        let limit = resolve_limit(max_results);
        let results = picker.fuzzy_search_directories(
            &parsed,
            FuzzySearchOptions {
                max_threads: resolve_max_threads(max_threads),
                current_file: None,
                project_path: Some(picker.base_path()),
                combo_boost_score_multiplier: 0,
                min_combo_count: 0,
                pagination: PaginationArgs { offset: 0, limit },
            },
        );
        let vals: Vec<Value<'_>> = results
            .items
            .iter()
            .map(|item| item.relative_path(picker).into_lisp(env))
            .collect::<Result<_>>()?;
        env.list(&vals)
    })
}

/// Record a file visit for frecency. Returns t when a DB took it,
/// nil when frecency is in-memory. Errors on real DB failures.
#[defun]
fn track_access(_env: &Env, id: i64, file: String) -> Result<bool> {
    let frecency: SharedFrecency = with_session(id, |s| Ok(s.frecency.clone()))?;
    let guard = frecency.read().map_err(|e| anyhow::anyhow!("fff: {e}"))?;
    match guard.as_ref() {
        None => Ok(false),
        Some(tracker) => {
            tracker
                .track_access(std::path::Path::new(&file))
                .map_err(|e| anyhow::anyhow!("fff: track-access failed for {file}: {e}"))?;
            Ok(true)
        }
    }
}

/// Record that QUERY in this session picked FILE, for combo boost.
/// Returns t when a DB took it, nil when history is in-memory.
#[defun]
fn track_query_completion(_env: &Env, id: i64, query: String, file: String) -> Result<bool> {
    let (tracker, base) = with_session(id, |s| Ok((s.tracker.clone(), s.base.clone())))?;
    let mut guard = tracker.write().map_err(|e| anyhow::anyhow!("fff: {e}"))?;
    match guard.as_mut() {
        None => Ok(false),
        Some(tracker) => {
            tracker
                .track_query_completion(
                    &query,
                    std::path::Path::new(&base),
                    std::path::Path::new(&file),
                )
                .map_err(|e| {
                    anyhow::anyhow!("fff: track-query-completion failed for {query}: {e}")
                })?;
            Ok(true)
        }
    }
}

fn parse_grep_mode(mode: Option<String>) -> Result<GrepMode> {
    match mode.as_deref() {
        None | Some("literal") => Ok(GrepMode::PlainText),
        Some("regex") => Ok(GrepMode::Regex),
        Some("fuzzy") => Ok(GrepMode::Fuzzy),
        Some(other) => Err(anyhow::anyhow!("fff: unknown grep mode {other}")),
    }
}

/// Parse QUERY for grep: `AiGrepConfig' when FILENAME-CONSTRAINT is
/// set, `GrepConfig' otherwise. `AiGrepConfig' is the pinned
/// `NvimGrepConfig' equivalent: same glob/path/git/location behavior
/// as `GrepConfig', plus `enable_filename_constraint', so bare
/// `main.rs'-looking tokens become `FilePath' filters instead of
/// search text.
fn parse_grep_query(query: &str, filename_constraint: Option<i64>) -> FFFQuery<'_> {
    if resolve_filename_constraint(filename_constraint) {
        FFFQuery::parse(query, AiGrepConfig)
    } else {
        FFFQuery::parse(query, GrepConfig)
    }
}

fn run_grep(
    id: i64,
    parsed: &FFFQuery,
    mode: Option<String>,
    max_matches: Option<i64>,
    time_budget_ms: Option<i64>,
    max_matches_per_file: Option<i64>,
    max_file_size: Option<i64>,
    enforce_time_budget: Option<i64>,
    file_offset: usize,
    smart_case: Option<i64>,
) -> Result<(usize, Vec<String>)> {
    let grep_mode = parse_grep_mode(mode)?;
    with_picker(id, |picker| {
        let limit = resolve_limit(max_matches);
        let budget = resolve_time_budget(time_budget_ms);
        let enforce = resolve_enforce_time_budget(enforce_time_budget);
        let smart = resolve_smart_case(smart_case);
        let make_options = |time_budget_ms: u64| GrepSearchOptions {
            page_limit: limit,
            mode: grep_mode,
            smart_case: smart,
            time_budget_ms,
            max_matches_per_file: resolve_max_matches_per_file(max_matches_per_file),
            max_file_size: resolve_max_file_size(max_file_size),
            file_offset,
            ..Default::default()
        };
        let mut result = picker.grep(parsed, &make_options(budget));
        // Pinned fff-search 0.10.6 predates upstream `enforce_time_budget'
        // (#827): plain/regex budgets already stay dormant until a match
        // exists, which is the off behavior. Emulate off explicitly so a
        // future crate bump keeps it: zero-match queries scan everything.
        if !enforce && budget != 0 && result.matches.is_empty() {
            result = picker.grep(parsed, &make_options(0));
        }
        let mut out = Vec::with_capacity(result.matches.len());
        for m in &result.matches {
            let file = result.files[m.file_index];
            let rel = file.relative_path(picker);
            // \x1f won't appear in paths. Content may hold it, so Elisp
            // takes the last segment as ranges. First four fields stable.
            let ranges = m
                .match_byte_offsets
                .iter()
                .map(|(s, e)| format!("{s}-{e}"))
                .collect::<Vec<_>>()
                .join(",");
            out.push(format!(
                "{rel}\x1f{}\x1f{}\x1f{}\x1f{ranges}",
                m.line_number, m.col, m.line_content
            ));
        }
        Ok((result.next_file_offset, out))
    })
}

fn grep_with_query<'a>(
    env: &'a Env,
    id: i64,
    parsed: &FFFQuery,
    mode: Option<String>,
    max_matches: Option<i64>,
    time_budget_ms: Option<i64>,
    max_matches_per_file: Option<i64>,
    max_file_size: Option<i64>,
    enforce_time_budget: Option<i64>,
    smart_case: Option<i64>,
) -> Result<Value<'a>> {
    let (_next, strings) = run_grep(
        id,
        parsed,
        mode,
        max_matches,
        time_budget_ms,
        max_matches_per_file,
        max_file_size,
        enforce_time_budget,
        0,
        smart_case,
    )?;
    let vals: Vec<Value<'_>> = strings
        .iter()
        .map(|s| s.clone().into_lisp(env))
        .collect::<Result<_>>()?;
    env.list(&vals)
}

/// MODE is "literal" (default), "regex", or "fuzzy".
/// QUERY is parsed for constraints and exclusions.
/// FILENAME-CONSTRAINT non-nil parses with `AiGrepConfig' (bare
/// `main.rs' tokens filter files); nil uses `GrepConfig'.
/// SMART-CASE nil/omitted means on (smart-case); 0 means always
/// case-sensitive, like `isearch-toggle-case-fold' off.
/// Returns "rel\x1fline\x1fcol\x1fcontent\x1franges" strings, where col is a
/// 0-based byte column and ranges are "start-end,..." byte offsets into
/// the content. Legacy shape: `grep-ex' carries the extra knobs.
#[defun]
fn grep(
    env: &Env,
    id: i64,
    query: String,
    mode: Option<String>,
    max_matches: Option<i64>,
    time_budget_ms: Option<i64>,
    filename_constraint: Option<i64>,
    smart_case: Option<i64>,
) -> Result<Value<'_>> {
    let parsed = parse_grep_query(&query, filename_constraint);
    grep_with_query(
        env,
        id,
        &parsed,
        mode,
        max_matches,
        time_budget_ms,
        None,
        None,
        None,
        smart_case,
    )
}

/// Extended `grep' with per-file cap, file size, and budget enforcement.
/// MAX-MATCHES-PER-FILE 0 means unlimited (export re-run, like nvim
/// quickfix with 0); omitted means 100. MAX-FILE-SIZE is bytes, 0 means
/// 10 MB. ENFORCE-TIME-BUDGET non-zero applies the budget to zero-match
/// searches too; nil scans everything when nothing matched.
/// FILENAME-CONSTRAINT non-zero parses with `AiGrepConfig' instead of
/// `GrepConfig' (see `grep'). SMART-CASE nil/omitted means on; 0 means
/// always case-sensitive (see `grep').
#[defun]
fn grep_ex(
    env: &Env,
    id: i64,
    query: String,
    mode: Option<String>,
    max_matches: Option<i64>,
    time_budget_ms: Option<i64>,
    max_matches_per_file: Option<i64>,
    max_file_size: Option<i64>,
    enforce_time_budget: Option<i64>,
    filename_constraint: Option<i64>,
    smart_case: Option<i64>,
) -> Result<Value<'_>> {
    let parsed = parse_grep_query(&query, filename_constraint);
    grep_with_query(
        env,
        id,
        &parsed,
        mode,
        max_matches,
        time_budget_ms,
        max_matches_per_file,
        max_file_size,
        enforce_time_budget,
        smart_case,
    )
}

/// Paged `grep-ex': one `file_offset' page per call, for Consult scroll
/// loading. Same knobs as `grep-ex' (including FILENAME-CONSTRAINT),
/// plus FILE-OFFSET to continue a previous page (0 starts over) and
/// MAX-MATCHES as the page size. Returns `(NEXT-OFFSET MATCH ...)',
/// where NEXT-OFFSET is `GrepResult::next_file_offset' for the next
/// call, 0 when no files remain. Pages are disjoint by file: the
/// offset indexes the filtered file list, and the limit always
/// finishes the current file before stopping.
/// SMART-CASE nil/omitted means on; 0 means always case-sensitive
/// (see `grep').
#[defun]
fn grep_page(
    env: &Env,
    id: i64,
    query: String,
    mode: Option<String>,
    max_matches: Option<i64>,
    time_budget_ms: Option<i64>,
    max_matches_per_file: Option<i64>,
    max_file_size: Option<i64>,
    enforce_time_budget: Option<i64>,
    filename_constraint: Option<i64>,
    file_offset: Option<i64>,
    smart_case: Option<i64>,
) -> Result<Value<'_>> {
    let parsed = parse_grep_query(&query, filename_constraint);
    let (next, strings) = run_grep(
        id,
        &parsed,
        mode,
        max_matches,
        time_budget_ms,
        max_matches_per_file,
        max_file_size,
        enforce_time_budget,
        resolve_file_offset(file_offset),
        smart_case,
    )?;
    let mut vals: Vec<Value<'_>> = Vec::with_capacity(strings.len() + 1);
    vals.push((next as i64).into_lisp(env)?);
    for s in &strings {
        vals.push(s.clone().into_lisp(env)?);
    }
    env.list(&vals)
}

fn escaped_verbatim(query: &str) -> std::borrow::Cow<'_, str> {
    // `grep_text()' drops one `\` before `*/!`. Double it.
    match query.as_bytes() {
        [b'\\', b'*' | b'/' | b'!', ..] => std::borrow::Cow::Owned(format!("\\{query}")),
        _ => std::borrow::Cow::Borrowed(query),
    }
}

fn verbatim_parsed(verbatim: &str) -> FFFQuery<'_> {
    FFFQuery {
        raw_query: verbatim,
        constraints: Vec::new(),
        fuzzy_query: FuzzyQuery::Text(verbatim),
        location: None,
    }
}

/// QUERY verbatim, no parse. For machine patterns.
#[defun]
fn grep_raw(
    env: &Env,
    id: i64,
    query: String,
    mode: Option<String>,
    max_matches: Option<i64>,
    time_budget_ms: Option<i64>,
    smart_case: Option<i64>,
) -> Result<Value<'_>> {
    let verbatim = escaped_verbatim(&query);
    let parsed = verbatim_parsed(&verbatim);
    grep_with_query(
        env,
        id,
        &parsed,
        mode,
        max_matches,
        time_budget_ms,
        None,
        None,
        None,
        smart_case,
    )
}

/// Extended `grep-raw' with the `grep-ex' knobs.
#[defun]
fn grep_raw_ex(
    env: &Env,
    id: i64,
    query: String,
    mode: Option<String>,
    max_matches: Option<i64>,
    time_budget_ms: Option<i64>,
    max_matches_per_file: Option<i64>,
    max_file_size: Option<i64>,
    enforce_time_budget: Option<i64>,
    smart_case: Option<i64>,
) -> Result<Value<'_>> {
    let verbatim = escaped_verbatim(&query);
    let parsed = verbatim_parsed(&verbatim);
    grep_with_query(
        env,
        id,
        &parsed,
        mode,
        max_matches,
        time_budget_ms,
        max_matches_per_file,
        max_file_size,
        enforce_time_budget,
        smart_case,
    )
}

/// Start a background rescan. Returns t.
#[defun]
fn rescan(_env: &Env, id: i64) -> Result<bool> {
    with_session(id, |s| {
        s.picker
            .trigger_full_rescan_async(&s.frecency)
            .map_err(|e| anyhow::anyhow!("fff: rescan failed: {e}"))?;
        Ok(true)
    })
}

/// Drop a session. Returns t if it existed.
#[defun]
fn destroy(_env: &Env, id: i64) -> Result<bool> {
    let mut sessions = SESSIONS.lock().map_err(|_| lock_err())?;
    Ok(sessions.remove(&id).is_some())
}

/// Drop a session and delete its on-disk DB dirs (best effort).
/// Returns the deleted dirs. The next `init' for the root re-creates
/// both session and DBs from the configured paths.
#[defun]
fn clear_caches(env: &Env, id: i64) -> Result<Value<'_>> {
    let session = SESSIONS
        .lock()
        .map_err(|_| lock_err())?
        .remove(&id)
        .ok_or_else(|| anyhow::anyhow!("fff: no session #{id}"))?;
    let mut deleted: Vec<Value<'_>> = Vec::new();
    for path in [
        session.frecency.destroy().ok().flatten(),
        session.tracker.destroy().ok().flatten(),
    ]
    .into_iter()
    .flatten()
    {
        deleted.push(path.to_string_lossy().into_owned().into_lisp(env)?);
    }
    env.list(&deleted)
}

/// Refresh git statuses without a full rescan. Returns the count.
#[defun]
fn refresh_git_status(_env: &Env, id: i64) -> Result<i64> {
    let (picker, frecency) = with_session(id, |s| Ok((s.picker.clone(), s.frecency.clone())))?;
    let count = picker
        .refresh_git_status(&frecency)
        .map_err(|e| anyhow::anyhow!("fff: git refresh failed: {e}"))?;
    Ok(count as i64)
}

fn db_line<T: DbHealthChecker>(inner: Option<&T>, label: &str, open_error: Option<&str>) -> String {
    match inner {
        Some(tracker) => match tracker.get_health() {
            Ok(health) => {
                let entries: u64 = health.entry_counts.iter().map(|(_, n)| n).sum();
                let state = if health.healthy {
                    "healthy"
                } else {
                    "unhealthy"
                };
                format!("{label}: {} ({entries} entries, {state})", health.path)
            }
            Err(e) => format!("{label}: health check failed: {e}"),
        },
        None => match open_error {
            Some(e) => format!("{label}: in-memory (open failed: {e})"),
            None => format!("{label}: in-memory (not configured)"),
        },
    }
}

/// One status line per DB: path plus entry counts when open, or the
/// in-memory fallback reason. Never errors.
#[defun]
fn db_status(env: &Env, id: i64) -> Result<Value<'_>> {
    let (frecency, tracker, frecency_error, tracker_error) = with_session(id, |s| {
        Ok((
            s.frecency.clone(),
            s.tracker.clone(),
            s.frecency_error.clone(),
            s.tracker_error.clone(),
        ))
    })?;
    let frecency_guard = frecency.read().map_err(|e| anyhow::anyhow!("fff: {e}"))?;
    let tracker_guard = tracker.read().map_err(|e| anyhow::anyhow!("fff: {e}"))?;
    let lines = [
        db_line(
            frecency_guard.as_ref(),
            "frecency",
            frecency_error.as_deref(),
        ),
        db_line(tracker_guard.as_ref(), "history", tracker_error.as_deref()),
    ];
    let vals: Vec<Value<'_>> = lines
        .iter()
        .map(|line| line.clone().into_lisp(env))
        .collect::<Result<_>>()?;
    env.list(&vals)
}

/// Base path of a session.
#[defun]
fn base_path(env: &Env, id: i64) -> Result<Value<'_>> {
    with_session(id, |s| s.base.clone().into_lisp(env))
}

/// Non-nil while the initial scan runs.
#[defun]
fn scanning_p(_env: &Env, id: i64) -> Result<bool> {
    with_picker(id, |picker| Ok(picker.is_scan_active()))
}

/// Live indexed file count.
#[defun]
fn file_count(_env: &Env, id: i64) -> Result<i64> {
    with_picker(id, |picker| Ok(picker.live_file_count() as i64))
}

/// Scan progress: `(SCANNED-COUNT IS-SCANNING)'.
/// SCANNED-COUNT is the walker's scanned-files counter
/// (`FilePicker::get_scan_progress'), IS-SCANNING non-nil while
/// the initial scan runs. Elisp polls this for the initial-wait
/// indicator, falling back to `scanning-p' / `file-count'.
#[defun]
fn scan_progress(env: &Env, id: i64) -> Result<Value<'_>> {
    with_picker(id, |picker| {
        let progress = picker.get_scan_progress();
        let vals: Vec<Value<'_>> = vec![
            (progress.scanned_files_count as i64).into_lisp(env)?,
            progress.is_scanning.into_lisp(env)?,
        ];
        env.list(&vals)
    })
}

#[emacs::module(name = "fff-core", defun_prefix = "fff--", separator = "")]
fn module_init(_env: &Env) -> Result<()> {
    Ok(())
}
