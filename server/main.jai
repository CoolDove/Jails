VERSION :: "0.2.22";

MEMORY_DEBUG :: false;

Args :: struct {
    verbose: bool;
    profile: bool;
    jai_path: string;
    jai_exe_name: string;
}

User_Options :: struct {
    auto_insert_parentheses: bool;
    use_symbols_from_local_modules: bool;
}

Server :: struct {
    args: Args;
    quit := false;

    // This is from the config file
    local_modules: [..]string;
    roots: []string;
    build_root: string;
    intermediate_path: string;

    project_root: string;
    files: Table(string, *Program_File);
    files_to_be_analyzed: [..]*Program_File;

    memory_files_lock: Mutex;
    memory_files: Table(string, [..]u8); // @todo: This is probably dumb - store files as array of lines instead (is this good idea?) // @ToDo: Keep track of a modified flag per file

    diagnostics_metaprogram_path: string;

    user_options: User_Options;
}

server: Server;

// @TODO: move this to somewhere else
is_hex_digit :: (c: u32) -> bool {
    return (c >= #char "a" && c <= #char "f") || (c >= #char "A" && c <= #char "F") || (c >= #char "0" && c <= #char "9");
}

// @TODO: move this to somewhere else
decode_url :: (url: string) -> string {
    out_url := copy_string(url);

    i, j: int;
    hex: [2]u8 = .[#char "0", #char "0"];

    while i < out_url.count {

        defer {
            i += 1;
            j += 1;
        }

        if out_url[i] != #char "%" || i + 2 >= out_url.count {
            out_url[j] = out_url[i];
            continue;
        }

        if is_hex_digit(out_url[i+1]) && is_hex_digit(out_url[i+2]) {
            hex[0] = out_url[i+1];
            hex[1] = out_url[i+2];

            out_url[j] = xx string_to_int(xx hex, 16);
            i += 2;
        } else {
            out_url[j] = out_url[i];
        }
    }
    out_url.count = j;

    return out_url;
}

find_entry_file :: (root: string) -> string {

    contains_main_procedure :: (file: string) -> bool {
        content, ok := read_entire_file(file, log_errors=false);
        if !ok return false;
        defer free(content);

        return contains(content, "main ::") || contains(content, "main::");
    }

    COMMON_MAIN_PATH :: string.["src/main.jai", "main.jai"]; // Maybe build.jai ... etc?

    for path: COMMON_MAIN_PATH {
        full_path := join(root, path, separator = "/",, temp);
        if file_exists(full_path) && contains_main_procedure(full_path) {
            return path;
        }
    }


    MAX_SCANNED_FILES :: 5000;

    Scan :: struct {
        candidates: [..]string;
        scanned: int;
    }

    visit :: (info: *File_Visit_Info, scan: *Scan) {
        if info.is_directory {
            if starts_with(info.short_name, ".") || info.short_name == "node_modules" { // Probably silly... But you know...
                info.descend_into_directory = false;
            }
            return;
        }

        if scan.scanned >= MAX_SCANNED_FILES return;
        if !ends_with(info.full_name, ".jai") return;

        scan.scanned += 1;
        array_add(*scan.candidates, copy_string(info.full_name));
    }

    scan: Scan;
    scan.candidates.allocator = temp;
    {
        push_allocator(temp);
        visit_files(root, true, *scan, visit, visit_files=true, visit_directories=true);
    }

    for file: scan.candidates {
        if contains(file, "main.jai") && contains_main_procedure(file) {
            return replace(file, root, "");
        }
    }

    for file: scan.candidates {
        if contains_main_procedure(file) {
            return replace(file, root, "");
        }
    }

    return ""; // TODO: What we gonna do when we do not find any?
}

#if OS == .WINDOWS {
    DEFAULT_JAI_EXE_NAME :: "jai.exe";
} else #if OS == .MACOS {
    DEFAULT_JAI_EXE_NAME :: "jai-macos";
} else #if OS == .LINUX {
    DEFAULT_JAI_EXE_NAME :: "jai-linux";
}

find_jai_path :: (executable_name: string) -> string, string {
    result: Process_Result;
    raw_path: string;
    err: string;

    #if OS == .WINDOWS {
        result, raw_path, err = run_command("where.exe", executable_name, capture_and_return_output=true);
        path_overwrite_separators(raw_path, #char "/");
        raw_path = trim(raw_path);

        // `where` prints every match on its own line, the first one wins.
        newline := find_index_from_left(raw_path, #char "\n");
        if newline >= 0 raw_path = slice(raw_path, 0, newline);
        raw_path = trim_right(raw_path, "\r");
    } else {
        #if OS == .MACOS {
            result, raw_path, err = run_command("whereis", "-q", executable_name, capture_and_return_output=true);
        } else { // Linux does not have the -q argument.
            result, raw_path, err = run_command("whereis", executable_name, capture_and_return_output=true);
        }

        raw_path = replace(raw_path, tprint("%:", executable_name), "");
        raw_path = trim(raw_path);

        BUFFER_SIZE :: 4096;
        buf: [BUFFER_SIZE + 1] u8;

        length := readlink(to_c_string(raw_path), buf.data, BUFFER_SIZE);
        if length > 0 {
            resolved_path := to_string(buf.data);
            if begins_with(resolved_path, "/") {
                raw_path = resolved_path;
            }
        }
    }

    if raw_path.count == 0 {
        #if OS == .MACOS || OS == .LINUX {
            if executable_name == "jai" return "", "";
            path, exe_name := find_jai_path("jai");
            return path, exe_name;
        }

        return "", "";
    }

    while contains(raw_path, "//") raw_path = replace(raw_path, "//", "/");

    // The compiler found on PATH is often a launcher (e.g. a scoop shim), which lives in a different
    // folder than the Jai installation it points to.
    exe_path := resolve_executable_launcher(raw_path);
    path_overwrite_separators(exe_path, #char "/");

    root := jai_root_from_executable(exe_path);
    if root.count == 0 {
        log_error("Could not find a 'modules' folder next to the Jai compiler at '%'. Use '-jai_path' to point at the Jai installation.", exe_path);
        return "", "";
    }

    return root, executable_name;
}

// Package managers (scoop) install a small launcher into a `shims` folder and record the real
// executable in a `<name>.shim` file next to it.
resolve_executable_launcher :: (exe_path: string) -> string {
    #if OS != .WINDOWS {
        return exe_path;
    }

    name := basename(exe_path);
    if name.count == 0 return exe_path;

    shim_path := join(trim_right(path_strip_filename(exe_path), "/"), tprint("%.shim", name), separator="/");
    content, ok := read_entire_file(shim_path, log_errors=false);
    if !ok return exe_path;
    defer free(content);

    target := parse_shim_target(content);
    if target.count == 0 return exe_path;

    return target;
}

parse_shim_target :: (content: string) -> string {
    // Format: path = "C:\path\to\jai.exe"
    quote_start := find_index_from_left(content, #char "\"");
    if quote_start < 0 return "";

    rest := slice(content, xx (quote_start + 1), xx (content.count - quote_start - 1));
    quote_end := find_index_from_left(rest, #char "\"");
    if quote_end < 0 return "";

    return copy_string(slice(rest, 0, xx quote_end));
}

// Jai root is the folder that contains `modules`, usually the parent of the `bin` folder with the compiler.
jai_root_from_executable :: (exe_path: string) -> string {
    exe_dir := trim_right(path_strip_filename(exe_path), "/");
    if exe_dir.count == 0 return "";

    parent_dir := trim_right(path_strip_filename(exe_dir), "/");

    candidates: []string = .[ exe_dir, parent_dir ];

    for candidate: candidates {
        if candidate.count == 0 continue;

        is_dir, _ := is_directory(join(candidate, "modules", separator="/"));
        if is_dir return candidate;
    }

    return "";
}

// Module imports can't be resolved without the Jai installation, tell the user instead of failing silently.
validate_jai_path :: () {
    if server.args.jai_path.count == 0 {
        log_error("Jai installation was not found, symbols from imported modules won't resolve. Pass '-jai_path <jai root>' or set 'jai_path' in jails.json.");
        return;
    }

    is_dir, _ := is_directory(join(server.args.jai_path, "modules", separator="/"));
    if !is_dir {
        log_error("'%' does not look like a Jai installation (no 'modules' folder), symbols from imported modules won't resolve. Check '-jai_path' or 'jai_path' in jails.json.", server.args.jai_path);
    }
}

Config_File :: struct {
    local_modules: []string;
    roots: []string;
    build_root: string;
    jai_path: string;
    intermediate_path: string;
    auto_insert_parentheses := true;
    use_symbols_from_local_modules := true;
}

load_config_file :: () {
    config_path := join(server.project_root, "/jails.json");
    if !file_exists(config_path) {
        log_error("Failed to find 'jails.json' in your project root. Tried: %", config_path);
        return;
    }
    log("Attempting to load config file: %", config_path);

    config_content := read_entire_file(config_path);

    ok, config := json_parse_string(config_content, Config_File);
    if !ok {
        log("Failed to parse config file %", config_path);
        return;
    }
    log("Loaded config file %", config_path);

    for config.local_modules {
        array_add_if_unique(*server.local_modules, it);
    }

    server.roots = config.roots;
    server.build_root = config.build_root;
    server.intermediate_path = config.intermediate_path;
    server.user_options.auto_insert_parentheses = config.auto_insert_parentheses;
    server.user_options.use_symbols_from_local_modules = config.use_symbols_from_local_modules;

    if config.jai_path.count > 0 {
        server.args.jai_path = config.jai_path;
    }
}

// User cache directory that holds everything Jails generates at runtime. The Jails installation
// directory is often read-only (Program Files, extension folders, package manager shims), so nothing
// is written next to the executable. Set JAILS_CACHE_DIR to override.
jails_cache_directory :: () -> string {
    override := environment_variable("JAILS_CACHE_DIR");
    if override.count > 0 {
        directory := copy_string(override); // Copy: the environment block is not ours to modify.
        path_overwrite_separators(directory, #char "/");
        return directory;
    }

    #if OS == .WINDOWS {
        local_app_data := environment_variable("LOCALAPPDATA");
        if local_app_data.count > 0 {
            directory := join(local_app_data, "jails", separator="/");
            path_overwrite_separators(directory, #char "/");
            return directory;
        }
    }

    #if OS == .MACOS {
        home := environment_variable("HOME");
        if home.count > 0 {
            directory := join(home, "Library", "Caches", "jails", separator="/");
            path_overwrite_separators(directory, #char "/");
            return directory;
        }
    }

    #if OS != .WINDOWS && OS != .MACOS {
        xdg_cache_home := environment_variable("XDG_CACHE_HOME");
        if xdg_cache_home.count > 0 {
            directory := join(xdg_cache_home, "jails", separator="/");
            path_overwrite_separators(directory, #char "/");
            return directory;
        }

        home := environment_variable("HOME");
        if home.count > 0 {
            directory := join(home, ".cache", "jails", separator="/");
            path_overwrite_separators(directory, #char "/");
            return directory;
        }
    }

    // No usable user cache directory: stay relative to the working directory rather than failing.
    return join(get_working_directory(), ".jails-cache", separator="/");
}

environment_variable :: (name: string) -> string {
    #if OS == .WINDOWS {
        // getenv() goes through the ANSI code page, which mangles paths with non-ASCII user names.
        wide_name, _, _ := utf8_to_wide(name);
        value_wide := _wgetenv(cast(*s16) wide_name);
        free(wide_name);

        if value_wide == null return "";

        value, success := wide_to_utf8(cast(*u16) value_wide);
        return ifx success then value else "";
    } else {
        value := getenv(temp_c_string(name));
        if value == null return "";
        return to_string(value);
    }
}

// Directory name for a project: a readable prefix plus the hash of the full project path, so that
// different projects never share (and overwrite) the same generated files. Deliberately not keyed by
// the Jails version: an upgrade reuses the same folder, and the content check below refreshes it.
metaprogram_cache_key :: (project_root: string) -> string {
    name_buffer: [..]u8;
    defer array_free(name_buffer);

    MAX_NAME_LENGTH :: 32;

    project_name := path_filename(project_root);
    for c: project_name {
        if name_buffer.count >= MAX_NAME_LENGTH break;

        safe := c == #char "-" || c == #char "_" || c == #char "." ||
                (c >= #char "a" && c <= #char "z") ||
                (c >= #char "A" && c <= #char "Z") ||
                (c >= #char "0" && c <= #char "9");
        array_add(*name_buffer, ifx safe then c else #char "_");
    }

    if name_buffer.count == 0 array_add(*name_buffer, #char "_");

    project_name_part := string.{ xx name_buffer.count, name_buffer.data };
    project_hash := fnv1a_hash(project_root.data, xx project_root.count);

    return sprint("%-%", project_name_part, project_hash);
}

create_diagnostics_metaprogram :: () -> bool, string {
    METAPROGRAM_SOURCE := #run -> string {
        project_dir := join(path_strip_filename(#file), "..");
        metaprogram_dir := tprint("%/metaprogram", project_dir);
        return read_entire_file(tprint("%/jails_diagnostics.jai", metaprogram_dir));
    };

    if METAPROGRAM_SOURCE.count == 0 {
        return false, "the bundled metaprogram source is empty";
    }

    if server.project_root.count == 0 {
        return false, "the project root is unknown";
    }

    cache_directory := jails_cache_directory();
    defer free(cache_directory);

    metaprogram_dir := sprint("%/projects/%/metaprogram", cache_directory, metaprogram_cache_key(server.project_root));

    if !make_directory_if_it_does_not_exist(metaprogram_dir, recursive=true) {
        message := sprint("could not create the directory '%'", metaprogram_dir);
        free(metaprogram_dir);
        return false, message;
    }

    metaprogram_source_file_path := tprint("%/jails_diagnostics.jai", metaprogram_dir);

    existing_source, has_existing_source := read_entire_file(metaprogram_source_file_path, log_errors=false);
    if has_existing_source {
        up_to_date := existing_source == METAPROGRAM_SOURCE;
        free(existing_source);
        if up_to_date {
            server.diagnostics_metaprogram_path = metaprogram_dir;
            log("Diagnostics metaprogram is up to date: %", metaprogram_source_file_path);
            return true, "";
        }
    }

    if !write_entire_file(metaprogram_source_file_path, METAPROGRAM_SOURCE) {
        message := sprint("could not write '%'", metaprogram_source_file_path);
        free(metaprogram_dir);
        return false, message;
    }

    server.diagnostics_metaprogram_path = metaprogram_dir;
    log("Diagnostics metaprogram written to %", metaprogram_source_file_path);
    return true, "";
}

get_root_from_initialization :: (body: LSP_Request_Message_Initialize) -> (bool, string) {
    if body.params.workspaceFolders != null && body.params.workspaceFolders.count > 0 {
        log("Workspace folders: %", body.params.workspaceFolders.*);
        uri: string = body.params.workspaceFolders.*[0].uri;
        if (uri.count > 0) {
            return true, normalize_and_copy_path(uri);
        }
    }

    if body.params.rootUri != null {
        log("Root URI: %", body.params.rootUri.*);
        uri := body.params.rootUri.*;
        if (uri.count > 0) {
            return true, normalize_and_copy_path(uri);
        }
    }

    if body.params.rootPath != null {
        log("Root Path: %", body.params.rootPath.*);
        uri := body.params.rootPath.*;
        if (uri.count > 0) {
            return true, normalize_and_copy_path(uri);
        }
    }

    return false, "";
}

handle_request :: (request: LSP_Request_Message, raw_request: string) {
    // log("Got %", request.method);

    if request.method == {
        case "shutdown";
            server.quit = true;
            lsp_respond(request.id, null);
        case "initialize";
            log("Jails version: %", VERSION);

            success, body := json_parse_string(raw_request, LSP_Request_Message_Initialize,, temp);
            if !success {
                log_error("Unable to parse initialize message");
                // @Incomplete: report error back!
                return;
            }

            // The order of precedence for establishing our project_root is:
            // - workspaceFolder (initially we only use the first path specified)
            // - rootUri
            // - rootPath
            // - cwd

            gotRootFromInitialization, rootFromInitialization := get_root_from_initialization(body);
            if gotRootFromInitialization {
                server.project_root = rootFromInitialization;
            } else {
                log_error("Unable to determine the project root!");

                cwd := get_working_directory();
                if (cwd.count > 0) {
                    // This isn't inherently erroneous, but this is useful info for the error log.
                    log_error("Falling back to the current working directory as the project root.");
                    server.project_root = normalize_and_copy_path(cwd);
                } else {
                    log_error("Unable to fallback to the current working directory as the project root. Exiting.");
                    return;
                }
            }

            log("Project Root: %", server.project_root);

            // Needs the project root: generated files are kept per project.
            metaprogram_ok, metaprogram_error := create_diagnostics_metaprogram();

            load_config_file();
            validate_jai_path();

            if server.build_root.count == 0 {
                server.build_root = find_entry_file(server.project_root);
            }

            if server.roots.count == 0 {
                roots := NewArray(1, string);
                roots[0] = ifx server.build_root.count > 0 then server.build_root else find_entry_file(server.project_root);
                server.roots = roots;
            }

            // Include $workingDirectory/modules as a default local module path (like a compiler)
            array_add_if_unique(*server.local_modules, "modules");

            log("Server.local_modules: %", server.local_modules);
            log("Server.roots: %", server.roots);
            log("Server.intermediate_path: %", server.intermediate_path);

            for root: server.roots {
                absolute_path: string;
                if starts_with(root, "/") {
                    absolute_path = root;
                } else {
                    absolute_path = join(server.project_root, root, separator="/",, temp);
                }

                absolute_path = normalize_path(absolute_path,, temp);
                parse_file(absolute_path, true);
            }

            // Parse Runtime Support and Preload
            {
                parse_file(tprint("%/modules/Runtime_Support.jai", server.args.jai_path), true);
                parse_file(tprint("%/modules/Preload.jai", server.args.jai_path), true);
            }


            analyze_files();

            to_send: LSP_Result_Initialize;
            lsp_respond(body.id, to_send);

            if !metaprogram_ok {
                log_error("Diagnostics are disabled: the metaprogram could not be set up: %", metaprogram_error);
                lsp_window_message("Jails: diagnostics are unavailable because the metaprogram could not be written: %. Set JAILS_CACHE_DIR to a writable directory and reload the window.", metaprogram_error, message_type = LSP_Window_Show_Message_Type.ERROR);
            }

            run_diagnostics();

        case "textDocument/definition";
            success, body := json_parse_string(raw_request, LSP_Request_Message_Definition,, temp);
            if !success {
                log_error("Unable to parse textDocument/definition message");
                return;
            }

            handle_goto(body);

        case "textDocument/didSave";
            success, body := json_parse_string(raw_request, LSP_Did_Save_Text_Document,, temp);
            if !success {
                log_error("Unable to parse textDocument/divSave message");
                return;
            }

            run_diagnostics();

            file_path := normalize_path(body.params.textDocument.uri,, temp);

            parse_file(file_path, true);
            analyze_files();

        case "textDocument/didOpen";
            success, body := json_parse_string(raw_request, LSP_Did_Open_Text_Document,, temp);
            if !success {
                log_error("Unable to parse textDocument/didOpen message");
                return;
            }

            file_path := normalize_path(body.params.textDocument.uri,, temp);

            content := body.params.textDocument.text;

            create_memory_file(file_path, content);

            parse_file(file_path);
            analyze_files();
            // run_diagnostics();

            log("File_path: %", file_path);

        case "textDocument/didChange";
            success, body := json_parse_string(raw_request, LSP_Did_Change_Text_Document,, temp);
            if !success {
                log_error("Unable to parse textDocument/didChange message");
                return;
            }

            file_path := normalize_path(body.params.textDocument.uri,, temp);
            edit_memory_file(file_path, body.params.contentChanges);
            parse_file(file_path, true);
            analyze_files();

        case "textDocument/documentSymbol";
            success, body := json_parse_string(raw_request, LSP_Request_Message_Document_Symbol,, temp);
            if !success {
                log_error("Unable to parse textDocument/documentSymbol message");
                return;
            }

            log("workspace/documentSymbol");

            handle_document_symbol(body);

        case "workspace/symbol";
            success, body := json_parse_string(raw_request, LSP_Request_Message_Workspace_Symbol,, temp);
            if !success {
                log_error("Unable to parse workspace/symbol message");
                return;
            }

            log("workspace/symbol");

            handle_workspace_symbol(body);

        case "textDocument/didClose";
            success, body := json_parse_string(raw_request, LSP_Did_Close_Text_Document,, temp);
            if !success {
                log_error("Unable to parse textDocument/dicClose message");
                return;
            }

            file_path := normalize_path(body.params.textDocument.uri,, temp);
            remove_memory_file(file_path);

        case "textDocument/completion";
            success, body := json_parse_string(raw_request, LSP_Request_Message_Completion,, temp);
            if !success {
                log_error("Unable to parse textDocument/completion message");
                return;
            }

            handle_completitions(body);

        case "textDocument/signatureHelp";
            success, body := json_parse_string(raw_request, LSP_Request_Message_Signature_Help,, temp);
            if !success {
                log_error("Unable to parse textDocument/completion message");
                return;
            }

            handle_signature_help(body);

        case "textDocument/hover";
            success, body := json_parse_string(raw_request, LSP_Request_Message_Hover,, temp);
            if !success {
                log_error("Unable to parse textDocument/hover message");
                return;
            }

            handle_hover(body);
    }
}

main :: () {
    init_lsp_output();

    context.logger = lsp_log;

    success: bool;
    success, server.args = parse_arguments(Args);
    if !success {
        log_error("Could not parse arguments");
        exit(1);
    }

    if server.args.jai_path.count == 0 {
        server.args.jai_path, server.args.jai_exe_name = find_jai_path(DEFAULT_JAI_EXE_NAME);
        if !server.args.jai_path {
            log_error("Failed to resolve Jai path");
            // @TODO: Send this fatal error over LSP somehow?
        } else {
            log("Jai path: % (%)", server.args.jai_path, server.args.jai_exe_name);
        }
    }

    log("Args are %", server.args);

    defer deinit(*server.files);

    server.files_to_be_analyzed.allocator = context.default_allocator;

    init_memory_files();
    defer deinit_memory_files();

    init_diagnostics();
    start_diagnostics_thread();

    start_parent_watchdog();

    while !server.quit {
        body, success, closed := read_message_from_stdin(,, temp);
        if closed {
            // The editor closed our stdin. Without this we would spin here forever at 100% CPU.
            log("Input closed, shutting down.");
            break;
        }
        if !success {
            log("Failed to read message. Skipping.");
            continue;
        }

        parseOK, request := json_parse_string(body, LSP_Request_Message,, temp);
        if !parseOK {
            log_error("Unable to parse as json:\n%\n\n", body);
            continue;
        }

        handle_request(request, body);

        reset_temporary_storage();
        #if MEMORY_DEBUG report_memory_leaks();
    }

    log("Shutdown");
    exit(0);
}

#import "Compiler";
#import "Basic"()(MEMORY_DEBUGGER=MEMORY_DEBUG);
#import "Print_Vars";
#import "Command_Line";
#import "File";
#import "File_Utilities";
#import "jaison";
#import "unicode_utils";
#import "String";
#import "Shared_Memory_Channel";
#import "Hash_Table";
#import "Hash";
#import "Random";
#import "Process";
#import "Reflection";
#import "Thread";
#import "Pool";
#import "jai_parser";
#import "Default_Allocator";
#import "System";

#load "rpc.jai";
#load "lsp_interface.jai";
#load "program.jai";
#load "diagnostics.jai";
#load "memory_files.jai";

#load "completition.jai";
#load "goto.jai";
#load "signature_help.jai";
#load "hover.jai";
#load "symbols.jai";
#load "utils.jai";

#if OS == .WINDOWS {
    #import "Windows";
    #import "Windows_Utf8";
} else {
    #import "POSIX";
}

// Parent watchdog.
//
// The main thread can be blocked for a long time (e.g. in parse_files_drain), so it can't be trusted to notice
// that the editor went away. Previously a stuck parse meant jails outlived the editor forever, burning CPU.
// This thread polls the parent pid and exits as soon as we get re-parented (parent died).
parent_watchdog_thread: Thread;

start_parent_watchdog :: () {
    thread_init(*parent_watchdog_thread, parent_watchdog);
    thread_start(*parent_watchdog_thread);
}

#if OS == .WINDOWS {
    // The Windows module does not expose these, so we bind what we need ourselves.
    watchdog_kernel32 :: #library,system "kernel32";

    WATCHDOG_TH32CS_SNAPPROCESS :: 0x00000002;
    WATCHDOG_SYNCHRONIZE        :: 0x00100000;
    WATCHDOG_WAIT_FOREVER       :: 0xFFFFFFFF;

    Watchdog_PROCESSENTRY32W :: struct {
        dwSize:              u32;
        cntUsage:            u32;
        th32ProcessID:       u32;
        th32DefaultHeapID:   u64;
        th32ModuleID:        u32;
        cntThreads:          u32;
        th32ParentProcessID: u32;
        pcPriClassBase:      s32;
        dwFlags:             u32;
        szExeFile:           [260]u16;
    }

    watchdog_CreateToolhelp32Snapshot :: (dwFlags: u32, th32ProcessID: u32) -> HANDLE #foreign watchdog_kernel32 "CreateToolhelp32Snapshot";
    watchdog_Process32FirstW :: (hSnapshot: HANDLE, lppe: *Watchdog_PROCESSENTRY32W) -> s32 #foreign watchdog_kernel32 "Process32FirstW";
    watchdog_Process32NextW  :: (hSnapshot: HANDLE, lppe: *Watchdog_PROCESSENTRY32W) -> s32 #foreign watchdog_kernel32 "Process32NextW";
    watchdog_OpenProcess :: (dwDesiredAccess: u32, bInheritHandle: s32, dwProcessId: u32) -> HANDLE #foreign watchdog_kernel32 "OpenProcess";

    get_parent_process_id :: () -> u32 {
        my_pid := GetCurrentProcessId();

        snapshot := watchdog_CreateToolhelp32Snapshot(WATCHDOG_TH32CS_SNAPPROCESS, 0);
        if snapshot == INVALID_HANDLE_VALUE return 0;
        defer CloseHandle(snapshot);

        entry: Watchdog_PROCESSENTRY32W;
        entry.dwSize = size_of(Watchdog_PROCESSENTRY32W);

        if watchdog_Process32FirstW(snapshot, *entry) == 0 return 0;

        while true {
            if entry.th32ProcessID == my_pid return entry.th32ParentProcessID;
            if watchdog_Process32NextW(snapshot, *entry) == 0 break;
        }

        return 0;
    }

    parent_watchdog :: (thread: *Thread) -> s64 {
        parent_pid := get_parent_process_id();
        if parent_pid == 0 {
            log_error("Parent watchdog: could not determine the parent process, relying on stdin EOF only.");
            return 0;
        }

        handle := watchdog_OpenProcess(WATCHDOG_SYNCHRONIZE, 0, parent_pid);
        if handle == null {
            log_error("Parent watchdog: could not open the parent process %, relying on stdin EOF only.", parent_pid);
            return 0;
        }

        // Blocks until the parent process terminates.
        WaitForSingleObject(handle, WATCHDOG_WAIT_FOREVER);
        log("Parent process % is gone, exiting.", parent_pid);
        exit(0);

        return 0;
    }
} else {
    parent_watchdog :: (thread: *Thread) -> s64 {
        initial_parent := getppid();

        while true {
            sleep_milliseconds(1000);
            if getppid() != initial_parent {
                log("Parent process % is gone, exiting.", initial_parent);
                exit(0);
            }
        }
        return 0;
    }
}
