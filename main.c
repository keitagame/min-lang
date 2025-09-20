// A minimal "direct execution" parser/interpreter.
// - No AST: parsing functions evaluate and return values immediately.
// - Supports: integer literals, + - * / %, parentheses,
//             variables (assignment), print(expr); if/else, while, blocks.
// - Very small symbol table (linked list). Single source file for simplicity.
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

typedef enum {
    T_EOF,
    T_NUMBER,
    T_IDENT,
    T_PLUS, T_MINUS, T_MUL, T_DIV, T_MOD,
    T_LPAREN, T_RPAREN,
    T_LBRACE, T_RBRACE,
    T_SEMI, T_ASSIGN, T_COMMA,
    T_LT, T_GT, T_LE, T_GE, T_EQ, T_NE, T_FM,
    T_IF, T_ELSE, T_WHILE, T_PRINT,
    T_UNKNOWN
} TokenType;

typedef struct {
    TokenType type;
    long value;       // for numbers
    char *ident;      // for identifiers
    int line;
} Token;

const char *src;
size_t pos;
int line_no;

void error_at(int line, const char *fmt, ...) {
    va_list ap;
    fprintf(stderr, "[line %d] error: ", line);
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    va_end(ap);
    fprintf(stderr, "\n");
    exit(1);
}

// Simple symbol table: linked list of name->value
typedef struct Var {
    char *name;
    long val;
    struct Var *next;
} Var;
Var *vars = NULL;

Var *find_var(const char *name) {
    for (Var *v = vars; v; v = v->next) {
        if (strcmp(v->name, name) == 0) return v;
    }
    return NULL;
}
long get_var(const char *name, int line) {
    Var *v = find_var(name);
    if (!v) error_at(line, "undefined variable '%s'", name);
    return v->val;
}
void set_var(const char *name, long val) {
    Var *v = find_var(name);
    if (v) {
        v->val = val;
        return;
    }
    v = malloc(sizeof(Var));
    v->name = strdup(name);
    v->val = val;
    v->next = vars;
    vars = v;
}

// Lexer
void skip_ws() {
    while (src[pos]) {
        if (src[pos] == ' ' || src[pos] == '\t' || src[pos] == '\r') { pos++; continue; }
        if (src[pos] == '\n') { line_no++; pos++; continue; }
        if (src[pos] == '/' && src[pos+1] == '/') { // line comment
            pos += 2;
            while (src[pos] && src[pos] != '\n') pos++;
            continue;
        }
        break;
    }
}

int starts_with(const char *s) {
    return strncmp(src + pos, s, strlen(s)) == 0;
}

Token next_token() {
    skip_ws();
    Token t = {T_EOF, 0, NULL, line_no};
    if (!src[pos]) { t.type = T_EOF; return t; }

    char c = src[pos];

    // numbers
    if (isdigit((unsigned char)c)) {
        long v = 0;
        while (isdigit((unsigned char)src[pos])) {
            v = v * 10 + (src[pos] - '0');
            pos++;
        }
        t.type = T_NUMBER;
        t.value = v;
        return t;
    }

    // identifiers / keywords
    if (isalpha((unsigned char)c) || c == '_') {
        size_t start = pos;
        pos++;
        while (isalnum((unsigned char)src[pos]) || src[pos] == '_') pos++;
        size_t len = pos - start;
        char *name = malloc(len + 1);
        memcpy(name, src + start, len);
        name[len] = '\0';
        if (strcmp(name, "if") == 0) { t.type = T_IF; free(name); return t; }
        if (strcmp(name, "else") == 0) { t.type = T_ELSE; free(name); return t; }
        if (strcmp(name, "while") == 0) { t.type = T_WHILE; free(name); return t; }
        if (strcmp(name, "print") == 0) { t.type = T_PRINT; free(name); return t; }
        t.type = T_IDENT;
        t.ident = name;
        return t;
    }

    // multi-char operators
    if (starts_with("==")) { pos += 2; t.type = T_EQ; return t; }
    if (starts_with("!=")) { pos += 2; t.type = T_NE; return t; }
    if (starts_with("<=")) { pos += 2; t.type = T_LE; return t; }
    if (starts_with(">=")) { pos += 2; t.type = T_GE; return t; }

    // single char
    pos++;
    switch (c) {
        case '+': t.type = T_PLUS; break;
        case '-': t.type = T_MINUS; break;
        case '*': t.type = T_MUL; break;
        case '/': t.type = T_DIV; break;
        case '%': t.type = T_MOD; break;
        case '(': t.type = T_LPAREN; break;
        case ')': t.type = T_RPAREN; break;
        case '{': t.type = T_LBRACE; break;
        case '}': t.type = T_RBRACE; break;
        case ';': t.type = T_SEMI; break;
        case ',': t.type = T_COMMA; break;
        case '=': t.type = T_ASSIGN; break;
        case '<': t.type = T_LT; break;
        case '>': t.type = T_GT; break;
        default:
            t.type = T_UNKNOWN;
            break;
    }
    return t;
}

// Parser with immediate evaluation
Token cur_tok;
void advance() {
    if (cur_tok.type == T_IDENT && cur_tok.ident) {
        free(cur_tok.ident);
        cur_tok.ident = NULL;
    }
    cur_tok = next_token();
}

// forward declarations
long parse_expression();
long parse_statement();

int accept(TokenType tt) {
    if (cur_tok.type == tt) { advance(); return 1; }
    return 0;
}
void expect(TokenType tt) {
    if (cur_tok.type != tt) {
        error_at(cur_tok.line, "expected token %d but got %d", tt, cur_tok.type);
    }
    advance();
}

// Precedence climbing for binary expressions
int is_binary_op(TokenType t) {
    switch (t) {
        case T_PLUS: case T_MINUS: case T_MUL: case T_DIV: case T_MOD:
        case T_LT: case T_GT: case T_LE: case T_GE: case T_EQ: case T_NE:
            return 1;
        default: return 0;
    }
}
int precedence(TokenType t) {
    switch (t) {
        case T_MUL: case T_DIV: case T_MOD: return 60;
        case T_PLUS: case T_MINUS: return 50;
        case T_LT: case T_GT: case T_LE: case T_GE: return 40;
        case T_EQ: case T_NE: return 30;
        default: return 0;
    }
}

long parse_primary() {
    if (cur_tok.type == T_NUMBER) {
        long v = cur_tok.value;
        advance();
        return v;
    }
    if (cur_tok.type == T_IDENT) {
        // Could be a variable or function call in future; here it's a variable.
        char *name = strdup(cur_tok.ident);
        int line = cur_tok.line;
        advance();
        return get_var(name, line);
    }
    if (accept(T_LPAREN)) {
        long v = parse_expression();
        expect(T_RPAREN);
        return v;
    }
    error_at(cur_tok.line, "unexpected token in primary");
    return 0;
}

long parse_binary_rhs(int expr_prec, long lhs) {
    while (1) {
        TokenType tok = cur_tok.type;
        int tok_prec = precedence(tok);
        if (tok_prec < expr_prec) return lhs;

        TokenType binop = tok;
        advance();
        long rhs = parse_primary();

        int next_prec = precedence(cur_tok.type);
        if (tok_prec < next_prec) {
            rhs = parse_binary_rhs(tok_prec + 1, rhs);
        }

        // apply binary op
        switch (binop) {
            case T_PLUS: lhs = lhs + rhs; break;
            case T_MINUS: lhs = lhs - rhs; break;
            case T_MUL: lhs = lhs * rhs; break;
            case T_DIV:
                if (rhs == 0) error_at(cur_tok.line, "division by zero");
                lhs = lhs / rhs; break;
            case T_MOD:
                if (rhs == 0) error_at(cur_tok.line, "mod by zero");
                lhs = lhs % rhs; break;
            case T_LT: lhs = lhs < rhs; break;
            case T_GT: lhs = lhs > rhs; break;
            case T_LE: lhs = lhs <= rhs; break;
            case T_GE: lhs = lhs >= rhs; break;
            case T_EQ: lhs = lhs == rhs; break;
            case T_NE: lhs = lhs != rhs; break;
            default: error_at(cur_tok.line, "unknown binary op");
        }
    }
}

long parse_expression() {
    // For simplicity: no assignment here. Assignment handled in statement level.
    long lhs = parse_primary();
    return parse_binary_rhs(0, lhs);
}

// Statements
long parse_block() {
    expect(T_LBRACE);
    while (cur_tok.type != T_RBRACE && cur_tok.type != T_EOF) {
        parse_statement();
    }
    expect(T_RBRACE);
    return 0;
}

long parse_statement() {
    if (cur_tok.type == T_PRINT) {
        advance();
        expect(T_LPAREN);
        long v = parse_expression();
        expect(T_RPAREN);
        expect(T_SEMI);
        printf("%ld\n", v);
        return 0;
    }
    if (cur_tok.type == T_IF) {
        advance();
        expect(T_LPAREN);
        long cond = parse_expression();
        expect(T_RPAREN);
        if (cond) {
            // then
            if (cur_tok.type == T_LBRACE) parse_block();
            else parse_statement();
        } else {
            // skip then
            // if there is an else, run it
            // Need to parse else only if present
            if (cur_tok.type == T_ELSE) {
                advance();
                if (cur_tok.type == T_LBRACE) parse_block();
                else parse_statement();
            }
        }
        return 0;
    }
    if (cur_tok.type == T_WHILE) {
        advance();
        expect(T_LPAREN);
        size_t checkpoint = pos;
        int saved_line = line_no;
        long cond = parse_expression();
        expect(T_RPAREN);

        // We need to execute the body repeatedly. Because we parse-once, we must
        // capture the source slice of the body and re-parse it each iteration.
        // For simplicity, we'll capture the substring starting at current pos for a single statement/block.
        // This is a simple approach; a production interpreter would build a reusable node or use offsets.
        // Save current parsing position to extract body text.
        size_t body_start = pos;
        int body_start_line = cur_tok.line;

        // Determine end of body by parsing it once into a temporary stream.
        // We'll copy the body source by counting braces if it's a block; otherwise, until the end of statement (semicolon).
        if (cur_tok.type == T_LBRACE) {
            // find matching brace
            int depth = 0;
            size_t p = pos;
            int lno = line_no;
            while (src[p]) {
                if (src[p] == '{') depth++;
                else if (src[p] == '}') {
                    depth--;
                    if (depth == 0) { p++; break; }
                }
                if (src[p] == '\n') lno++;
                p++;
            }
            size_t body_end = p;
            size_t len = body_end - body_start;
            char *body_src = malloc(len + 1);
            memcpy(body_src, src + body_start, len);
            body_src[len] = '\0';

            // now execute loop
            // We will evaluate condition and execute body by setting up parsing position to body_src.
            while (1) {
                // re-evaluate condition: reset outer lexing state to the condition's original location
                pos = checkpoint;
                line_no = saved_line;
                cur_tok = next_token();
                // evaluate condition
                expect(T_LPAREN); // condition already consumed earlier; here ensure we re-evaluate from same place
                // Actually, easier: we will re-evaluate by lexing condition from saved checkpoint:
                // Move pos to checkpoint, create a temporary src pointing to after '(' and before ')'.
                // To keep it simple: we recompute condition by scanning body_start - naive but acceptable for small examples.
                // Simpler approach: disallow complex while condition here and require integer literal or variable. But
                // to keep reasonable, we'll implement a safer approach: extract cond substring between parentheses from original source.

                // Extract condition substring from original source:
                // Find '(' before body_start (we know there was an '(' just before cond originally).
                size_t p1 = checkpoint; // points after '(' originally (we set checkpoint earlier)
                // find the matching ')' - naive scan
                size_t p2 = p1;
                int depth_p = 0;
                while (src[p2]) {
                    if (src[p2] == '(') depth_p++;
                    else if (src[p2] == ')') {
                        if (depth_p == 0) break;
                        depth_p--;
                    }
                    p2++;
                }
                size_t cond_len = p2 - p1;
                char *cond_src = malloc(cond_len + 1);
                memcpy(cond_src, src + p1, cond_len);
                cond_src[cond_len] = '\0';

                // setup temporary parsing state to evaluate condition
                const char *old_src = src;
                size_t old_pos = pos;
                int old_line = line_no;
                Token old_tok = cur_tok;

                src = cond_src;
                pos = 0;
                line_no = 1;
                cur_tok = next_token();
                long cval = parse_expression();
                if (cur_tok.type != T_EOF) {
                    // ignore
                }

                // restore
                src = old_src;
                pos = old_pos;
                line_no = old_line;
                cur_tok = old_tok;
                free(cond_src);

                if (!cval) {
                    free(body_src);
                    break;
                }

                // execute body by setting a temporary src pointer to body_src and parsing statements from it
                const char *real_src = src;
                size_t real_pos = pos;
                int real_line = line_no;
                Token real_tok = cur_tok;

                src = body_src;
                pos = 0;
                line_no = body_start_line;
                cur_tok = next_token();
                // parse body once (it will consume until EOF of body_src)
                if (cur_tok.type == T_LBRACE) {
                    parse_block(); // executes body
                } else {
                    parse_statement();
                }

                // restore
                src = real_src;
                pos = real_pos;
                line_no = real_line;
                cur_tok = real_tok;
            }
            return 0;
        } else {
            // single-statement body: find end at next semicolon (simple)
            size_t p = pos;
            while (src[p] && src[p] != ';') {
                if (src[p] == '\n') ; // continue
                p++;
            }
            if (src[p] == ';') p++;
            size_t body_end = p;
            size_t len = body_end - body_start;
            char *body_src = malloc(len + 1);
            memcpy(body_src, src + body_start, len);
            body_src[len] = '\0';

            // similar loop as above but for single stmt
            while (1) {
                // re-evaluate condition (extract substring between parentheses)
                size_t p1 = checkpoint;
                size_t p2 = p1;
                int depth_p = 0;
                while (src[p2]) {
                    if (src[p2] == '(') depth_p++;
                    else if (src[p2] == ')') {
                        if (depth_p == 0) break;
                        depth_p--;
                    }
                    p2++;
                }
                size_t cond_len = p2 - p1;
                char *cond_src = malloc(cond_len + 1);
                memcpy(cond_src, src + p1, cond_len);
                cond_src[cond_len] = '\0';

                const char *old_src = src;
                size_t old_pos = pos;
                int old_line = line_no;
                Token old_tok = cur_tok;

                src = cond_src;
                pos = 0;
                line_no = 1;
                cur_tok = next_token();
                long cval = parse_expression();

                src = old_src;
                pos = old_pos;
                line_no = old_line;
                cur_tok = old_tok;
                free(cond_src);

                if (!cval) { free(body_src); break; }

                // execute body
                const char *real_src = src;
                size_t real_pos = pos;
                int real_line = line_no;
                Token real_tok = cur_tok;

                src = body_src;
                pos = 0;
                line_no = body_start_line;
                cur_tok = next_token();
                parse_statement();

                src = real_src;
                pos = real_pos;
                line_no = real_line;
                cur_tok = real_tok;
            }
            return 0;
        }
    }

    if (cur_tok.type == T_LBRACE) {
        parse_block();
        return 0;
    }

    // assignment or expression-stmt
    if (cur_tok.type == T_IDENT) {
        // lookahead to see if assignment
        char *name = strdup(cur_tok.ident);
        int ln = cur_tok.line;
        advance();
        if (accept(T_ASSIGN)) {
            long v = parse_expression();
            expect(T_SEMI);
            set_var(name, v);
            free(name);
            return 0;
        } else {
            // not assignment: treat as expression starting with identifier (variable value already read)
            // We already consumed the ident; but parse_expression expects to start at that token.
            // For simplicity, evaluate as variable value and then require semicolon.
            long val = get_var(name, ln);
            free(name);
            expect(T_SEMI);
            (void)val; // expression statement does nothing
            return 0;
        }
    }

    // expression statement
    long v = parse_expression();
    expect(T_SEMI);
    (void)v;
    return 0;
}

void interpret(const char *text) {
    src = text;
    pos = 0;
    line_no = 1;
    cur_tok = next_token();
    while (cur_tok.type != T_EOF) {
        parse_statement();
    }
}

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s file.ml\n", argv[0]);
        return 1;
    }
    FILE *f = fopen(argv[1], "rb");
    if (!f) { perror("fopen"); return 1; }
    fseek(f, 0, SEEK_END);
    long sz = ftell(f);
    fseek(f, 0, SEEK_SET);
    char *buf = malloc(sz + 1);
    if (fread(buf, 1, sz, f) != (size_t)sz) { perror("fread"); return 1; }
    buf[sz] = '\0';
    fclose(f);

    interpret(buf);
    free(buf);
    return 0;
}
