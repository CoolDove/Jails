MAX_HOVER_MEMBERS      :: 12;
MAX_HOVER_VALUE_LENGTH :: 80;
MAX_HOVER_DOC_LINES    :: 20;

handle_hover :: (request: LSP_Request_Message_Hover) {
    push_allocator(temp);

    file_path := normalize_path(request.params.textDocument.uri);

    file := get_file(file_path);
    if !file {
        lsp_respond(request.id, null);
        return;
    }

    cursor_location := lsp_location_to_node_location(request.params.position, file_path);

    // Comments are not part of the AST and hovering them would be noise.
    if inside_comment(cursor_location) {
        lsp_respond(request.id, null);
        return;
    }

    cursor_node := get_node_by_location(file, cursor_location);
    if !cursor_node {
        lsp_respond(request.id, null);
        return;
    }

    decl := hover_find_declaration(cursor_node);
    if !decl {
        lsp_respond(request.id, null);
        return;
    }

    hover: LSP_Hover;
    hover.contents.kind = "markdown";
    hover.contents.value = hover_build_content(decl);

    lsp_respond(request.id, hover);
}

hover_find_declaration :: (node: *Node) -> *Declaration {
    if !node return null;

    // Declarations do not create identifier nodes for their names.
    if node.kind == .DECLARATION {
        return cast(*Declaration) node;
    }

    if node.kind != .IDENTIFIER {
        return null;
    }

    ident := cast(*Identifier) node;

    if ident.parent {
        // Never fall back to name based resolution for `object.member`, that would show a same named
        // local instead of the member.
        if hover_is_member_reference(ident) {
            return hover_member_declaration(ident);
        }

        // `Type.{ member = value }`
        decl := hover_struct_literal_member(ident);
        if decl return decl;

        // `procedure(named_argument = value)`
        decl = hover_named_argument(ident);
        if decl return decl;

        if ident.parent.kind == .PROCEDURE_CALL {
            proc_call := cast(*Procedure_Call) ident.parent;
            if proc_call.procedure == ident {
                decl = hover_procedure_declaration(ident, proc_call);
                if decl return decl;
            }
        }
    }

    decls := get_identifier_decls(ident);
    if decls.count == 0 {
        return null;
    }

    return decls[0];
}

// Is the identifier the right side of a `object.member` (or `object.method()`) expression?
hover_is_member_reference :: (ident: *Identifier) -> bool {
    if !ident.parent return false;

    if ident.parent.kind == .BINARY_OPERATION {
        binary_op := cast(*Binary_Operation) ident.parent;
        return binary_op.operation == .DOT && binary_op.right == ident;
    }

    if ident.parent.kind == .PROCEDURE_CALL {
        proc_call := cast(*Procedure_Call) ident.parent;
        if proc_call.procedure != ident return false;

        if !proc_call.parent || proc_call.parent.kind != .BINARY_OPERATION return false;

        return (cast(*Binary_Operation) proc_call.parent).operation == .DOT;
    }

    return false;
}

hover_member_declaration :: (ident: *Identifier) -> *Declaration {
    if !ident.parent return null;

    leaf_node: *Node;
    op: *Binary_Operation;

    if ident.parent.kind == .BINARY_OPERATION {
        op = cast(*Binary_Operation) ident.parent;
        leaf_node = ident;
    } else if ident.parent.kind == .PROCEDURE_CALL {
        proc_call := cast(*Procedure_Call) ident.parent;
        if proc_call.procedure != ident return null;

        if !proc_call.parent || proc_call.parent.kind != .BINARY_OPERATION return null;

        op = cast(*Binary_Operation) proc_call.parent;
        leaf_node = proc_call;
    }

    if !op || op.operation != .DOT {
        return null;
    }

    path := get_dot_path(op, leaf_node);
    if path.count < 2 {
        return null;
    }

    path = array_view(path, 0, path.count-1);

    type := get_path_type(path);
    if !type {
        return null;
    }

    return get_member_in(type, ident);
}

hover_struct_literal_member :: (ident: *Identifier) -> *Declaration {
    if !ident.parent || ident.parent.kind != .BINARY_OPERATION {
        return null;
    }

    binary_op := cast(*Binary_Operation) ident.parent;

    if binary_op.right == ident {
        return null;
    }

    if !binary_op.parent || binary_op.parent.kind != .LITERAL {
        return null;
    }

    literal := cast(*Literal) binary_op.parent;
    if literal.value_type != .STRUCT || !literal.struct_literal_info.type {
        return null;
    }

    type := get_node_type(literal.struct_literal_info.type);
    if !type {
        return null;
    }

    return get_member_in(type, ident);
}

hover_named_argument :: (ident: *Identifier) -> *Declaration {
    if !ident.parent || ident.parent.kind != .BINARY_OPERATION {
        return null;
    }

    binary_op := cast(*Binary_Operation) ident.parent;

    // Only the left side of `name = value` can be a named argument.
    if binary_op.right == ident {
        return null;
    }

    if !binary_op.parent || binary_op.parent.kind != .PROCEDURE_CALL {
        return null;
    }

    proc_call := cast(*Procedure_Call) binary_op.parent;
    if !proc_call.procedure || proc_call.procedure.kind != .IDENTIFIER {
        return null;
    }

    proc_decl := hover_procedure_declaration(cast(*Identifier) proc_call.procedure, proc_call);
    if !proc_decl || !proc_decl.expression || proc_decl.expression.kind != .PROCEDURE {
        return null;
    }

    procedure := cast(*Procedure) proc_decl.expression;

    for arg: procedure.arguments {
        unwrapped_arg := unwrap_polymorphic_constant(arg);
        if !unwrapped_arg || unwrapped_arg.kind != .DECLARATION continue;

        arg_decl := cast(*Declaration) unwrapped_arg;
        if arg_decl.name == ident.name {
            return arg_decl;
        }
    }

    return null;
}

// Picks the overload that matches the call arguments so hovering a call shows the procedure that is actually used.
hover_procedure_declaration :: (ident: *Identifier, proc_call: *Procedure_Call) -> *Declaration {
    decls := get_identifier_decls(ident);
    if decls.count == 0 {
        return null;
    }

    procedures: [..]*Procedure;
    procedure_decls: [..]*Declaration;

    for decl: decls {
        type := get_node_type(decl, stop_at_procedure=true);
        if !type || type.kind != .PROCEDURE continue;

        array_add(*procedures, cast(*Procedure) type);
        array_add(*procedure_decls, decl);
    }

    if procedure_decls.count == 0 {
        return null;
    }

    best := get_best_overload_for_procedure_call(procedures, proc_call);
    if best < 0 || best >= procedure_decls.count {
        return procedure_decls[0];
    }

    return procedure_decls[best];
}

hover_build_content :: (decl: *Declaration) -> string {
    builder: String_Builder;

    append(*builder, "```jai\n");
    hover_print_declaration(*builder, decl);
    append(*builder, "\n```");

    deprecated_note := hover_deprecated_note(decl);
    if deprecated_note.count > 0 {
        append(*builder, "\n\n**Deprecated:** ");
        append(*builder, deprecated_note);
    }

    // Procedure arguments live on the procedure's lines, they would inherit the procedure's documentation.
    if !decl.parent || decl.parent.kind != .PROCEDURE {
        doc := hover_doc_comment(decl);
        if doc.count > 0 {
            append(*builder, "\n\n");
            append(*builder, doc);
        }
    }

    return builder_to_string(*builder);
}

hover_print_declaration :: (builder: *String_Builder, decl: *Declaration) {
    append(builder, decl.name);

    if decl.expression {
        if decl.expression.kind == {
            case .PROCEDURE;
                append(builder, " :: ");
                hover_print_procedure(builder, cast(*Procedure) decl.expression);
                return;

            case .STRUCT;
                _struct := cast(*Struct) decl.expression;
                hover_print_type_body(builder, "struct", _struct.block, _struct.polymorphic_arguments);
                return;

            case .ENUM;
                _enum := cast(*Enum) decl.expression;
                hover_print_type_body(builder, ifx _enum.is_enum_flags then "enum_flags" else "enum", _enum.block);
                return;

            case .UNION;
                _union := cast(*Union) decl.expression;
                hover_print_type_body(builder, "union", _union.block, _union.polymorphic_arguments);
                return;
        }
    }

    if decl.type_inst {
        append(builder, ": ");
        ast_print(builder, decl.type_inst);
        return;
    }

    // Constants are compile-time values, showing the value is more useful than the inferred type.
    if decl.const {
        if decl.expression {
            preview := hover_expression_preview(decl.expression);
            if preview.count > 0 {
                append(builder, " :: ");
                append(builder, preview);
            }
        }
        return;
    }

    type_name := hover_type_name(get_node_type(decl));
    if type_name.count > 0 {
        append(builder, ": ");
        append(builder, type_name);
        return;
    }

    if decl.expression {
        preview := hover_expression_preview(decl.expression);
        if preview.count > 0 {
            append(builder, " = ");
            append(builder, preview);
        }
    }
}

hover_print_procedure :: (builder: *String_Builder, procedure: *Procedure) {
    append(builder, "(");

    for arg: procedure.arguments {
        if it_index > 0 append(builder, ", ");
        hover_print_argument(builder, arg);
    }

    append(builder, ")");

    if procedure.returns.count > 0 {
        append(builder, " -> ");

        multiple := procedure.returns.count > 1;
        if multiple append(builder, "(");

        for _return: procedure.returns {
            if it_index > 0 append(builder, ", ");
            if _return.must append(builder, "#must ");
            ast_print(builder, _return.expression);
        }

        if multiple append(builder, ")");
    }
}

// The parser's ast_print drops quotes from string literals, which makes default arguments unreadable.
hover_print_argument :: (builder: *String_Builder, arg: *Node) {
    if arg.kind != .DECLARATION {
        ast_print(builder, arg);
        return;
    }

    decl := cast(*Declaration) arg;
    append(builder, decl.name);

    if decl.type_inst {
        append(builder, ": ");
        ast_print(builder, decl.type_inst);
    }

    if decl.expression {
        append(builder, " = ");
        append(builder, hover_expression_preview(decl.expression));
    }
}

hover_print_type_body :: (builder: *String_Builder, keyword: string, block: *Block, polymorphic_arguments: []*Node = .[]) {
    append(builder, " :: ");
    append(builder, keyword);

    if polymorphic_arguments.count > 0 {
        append(builder, "(");
        for arg: polymorphic_arguments {
            if it_index > 0 append(builder, ", ");
            ast_print(builder, arg);
        }
        append(builder, ")");
    }

    if !block {
        append(builder, " {}");
        return;
    }

    member_count: int;
    for member: block.members {
        if !hover_is_printable_member(member) continue;
        member_count += 1;
    }

    append(builder, " {");

    printed: int;
    for member: block.members {
        if !hover_is_printable_member(member) continue;

        if printed >= MAX_HOVER_MEMBERS {
            append(builder, tprint("\n    ... % more", member_count - printed));
            break;
        }

        append(builder, "\n    ");

        if member.kind == .IDENTIFIER {
            append(builder, (cast(*Identifier) member).name);
        } else if member.kind == .USING {
            ast_print(builder, member);
        } else {
            hover_print_declaration(builder, cast(*Declaration) member);
        }

        append(builder, ";");
        printed += 1;
    }

    if printed > 0 append(builder, "\n");
    append(builder, "}");
}

hover_is_printable_member :: (node: *Node) -> bool {
    return node.kind == .DECLARATION || node.kind == .IDENTIFIER || node.kind == .USING;
}

hover_type_name :: (node: *Node) -> string {
    if !node return "";

    if node.kind == {
        case .IDENTIFIER;
            return (cast(*Identifier) node).name;

        case .STRUCT;
            return hover_type_declaration_name(node, "struct");

        case .ENUM;
            return hover_type_declaration_name(node, "enum");

        case .UNION;
            return hover_type_declaration_name(node, "union");

        case .ARRAY_TYPE;
            builder: String_Builder;
            ast_print(*builder, node);
            return builder_to_string(*builder);

        case .UNARY_OPERATION;
            unary_op := cast(*Unary_Operation) node;
            if unary_op.operation == .POINTER {
                inner := hover_type_name(unary_op.expression);
                if inner.count == 0 return "*void";
                return tprint("*%", inner);
            }

        case .LITERAL;
            literal := cast(*Literal) node;
            if literal.value_type == {
                case .STRUCT; return hover_type_name(literal.struct_literal_info.type);
                case .STRING; return "string";
                case .BOOL;   return "bool";
                case .FLOAT;  return "float";
                case .INT;    return "int";
                case;
            }
    }

    return "";
}

hover_type_declaration_name :: (node: *Node, fallback: string) -> string {
    if node.parent && node.parent.kind == .DECLARATION {
        return (cast(*Declaration) node.parent).name;
    }

    return fallback;
}

hover_deprecated_note :: (decl: *Declaration) -> string {
    if !decl.expression || decl.expression.kind != .PROCEDURE {
        return "";
    }

    return (cast(*Procedure) decl.expression).deprecated_note;
}

// Expression previews are kept on a single line and truncated, a hover should never become a wall of text.
hover_expression_preview :: (expression: *Node) -> string {
    if expression.kind == .LITERAL {
        literal := cast(*Literal) expression;
        if literal.value_type == .STRING {
            return tprint("\"%\"", literal._string);
        }
    }

    expression_builder: String_Builder;
    ast_print(*expression_builder, expression);

    text := builder_to_string(*expression_builder);

    newline := find_index_from_left(text, #char "\n");
    if newline >= 0 {
        text.count = newline;
    }

    text = trim_right(text, " \t");

    if text.count > MAX_HOVER_VALUE_LENGTH {
        count := MAX_HOVER_VALUE_LENGTH;

        // Never cut a multi byte UTF-8 character in half.
        while count > 0 && (text[count] & 0xC0) == 0x80 {
            count -= 1;
        }

        return tprint("%...", slice(text, 0, count));
    }

    return text;
}

// Jai has no doc comments, so we treat the comment block directly above a declaration as its documentation.
hover_doc_comment :: (decl: *Declaration) -> string {
    file := get_file(decl.location.file);
    if !file || file.comments.count == 0 {
        return "";
    }

    comment_indices: [..]int;
    defer array_free(comment_indices);

    expected_line := decl.location.l0 - 1;
    i := file.comments.count - 1;
    truncated := false;

    while i >= 0 && expected_line >= 0 {
        comment := file.comments[i];

        // Line comments end at the '\n', which the lexer reports as the start of the next line.
        last_line := comment.l1;
        if !comment.multiline && last_line > comment.l0 {
            last_line = comment.l0;
        }

        if last_line > expected_line {
            i -= 1;
            continue;
        }

        // A blank line between the comment and the declaration breaks the chain.
        if last_line != expected_line break;

        if comment_indices.count >= MAX_HOVER_DOC_LINES {
            truncated = true;
            break;
        }

        array_add(*comment_indices, i);
        expected_line = comment.l0 - 1;
        i -= 1;
    }

    if comment_indices.count == 0 {
        return "";
    }

    builder: String_Builder;
    first := true;

    if truncated {
        append(*builder, "...\n");
        first = false;
    }

    i = comment_indices.count - 1;
    while i >= 0 {
        comment := file.comments[comment_indices[i]];
        text := trim(comment.string_value);

        if text.count > 0 {
            if !first append(*builder, "\n");
            append(*builder, text);
            first = false;
        }

        i -= 1;
    }

    return builder_to_string(*builder);
}
