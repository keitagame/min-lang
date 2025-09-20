// lexer.c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

typedef enum {
    TK_EOF,
    TK_IDENT,
    TK_NUMBER,
    TK_STRING,
    TK_CHAR,
    TK_KEYWORD,
    TK_PLUS, TK_MINUS, TK_STAR, TK_SLASH,
    TK_EQ, TK_EQEQ, TK_NEQ, TK_LT, TK_LTE, TK_GT, TK_GTE,
    TK_AND, TK_OR, TK_NOT,
    TK_ASSIGN, // =
    TK_SEMI, TK_COMMA,
    TK_LPAREN, TK_RPAREN, TK_LBRACE, TK_RBRACE,
    TK_UNKNOWN
} TokenType;

typedef struct {
    TokenType type;
    char *lexeme;   // null-terminated string
    int line;
    int column;
} Token;

typedef struct {
    const char *src;
    size_t pos;
    size_t len;
    int line;
    int col;
} Lexer;

static const char *keywords[] = {
    "if", "else", "for", "while", "return", "int", "char", "void", "struct", NULL
};

int is_keyword(const char *s) {
    for (int i=0; keywords[i]; ++i)
        if (strcmp(s, keywords[i]) == 0) return 1;
    return 0;
}

Lexer lexer_init(const char *src) {
    Lexer L;
    L.src = src;
    L.pos = 0;
    L.len = strlen(src);
    L.line = 1;
    L.col = 1;
    return L;
}

int at_end(Lexer *L) { return L->pos >= L->len; }
char peek(Lexer *L) { return at_end(L) ? '\0' : L->src[L->pos]; }
char peek_next(Lexer *L) { return (L->pos + 1 >= L->len) ? '\0' : L->src[L->pos+1]; }
char advance(Lexer *L) {
    if (at_end(L)) return '\0';
    char c = L->src[L->pos++];
    if (c == '\n') { L->line++; L->col = 1; }
    else L->col++;
    return c;
}
void skip_whitespace(Lexer *L) {
    for (;;) {
        char c = peek(L);
        if (c == ' ' || c == '\r' || c == '\t' || c == '\n') {
            advance(L);
            continue;
        }
        // comments
        if (c == '/' && peek_next(L) == '/') {
            // line comment
            advance(L); advance(L);
            while (peek(L) != '\n' && !at_end(L)) advance(L);
            continue;
        }
        if (c == '/' && peek_next(L) == '*') {
            // block comment
            advance(L); advance(L);
            while (!(peek(L) == '*' && peek_next(L) == '/') && !at_end(L)) advance(L);
            if (!at_end(L)) { advance(L); advance(L); } // consume */
            continue;
        }
        break;
    }
}

char *make_lexeme(const char *start, size_t len) {
    char *s = (char*)malloc(len+1);
    memcpy(s, start, len);
    s[len] = '\0';
    return s;
}

Token make_token(TokenType type, const char *start, size_t len, int line, int col) {
    Token t;
    t.type = type;
    t.lexeme = make_lexeme(start, len);
    t.line = line;
    t.column = col;
    return t;
}

Token error_token(const char *msg, int line, int col) {
    return make_token(TK_UNKNOWN, msg, strlen(msg), line, col);
}

Token scan_identifier_or_keyword(Lexer *L) {
    size_t start_pos = L->pos;
    int start_col = L->col;
    while (isalpha((unsigned char)peek(L)) || peek(L) == '_' || isdigit((unsigned char)peek(L))) advance(L);
    size_t len = L->pos - start_pos;
    const char *start = L->src + start_pos;
    char *lexeme = make_lexeme(start, len);
    TokenType type = is_keyword(lexeme) ? TK_KEYWORD : TK_IDENT;
    Token t = make_token(type, start, len, L->line, start_col);
    free(lexeme); // make_token already duplicated substring, free temporary
    return t;
}

Token scan_number(Lexer *L) {
    size_t start_pos = L->pos;
    int start_col = L->col;
    while (isdigit((unsigned char)peek(L))) advance(L);
    // optional fractional part
    if (peek(L) == '.' && isdigit((unsigned char)peek_next(L))) {
        advance(L);
        while (isdigit((unsigned char)peek(L))) advance(L);
    }
    size_t len = L->pos - start_pos;
    const char *start = L->src + start_pos;
    return make_token(TK_NUMBER, start, len, L->line, start_col);
}

Token scan_string(Lexer *L) {
    char quote = advance(L); // consume " or '
    size_t start_pos = L->pos;
    int start_col = L->col;
    // collect until matching quote (allow escapes)
    while (!at_end(L) && peek(L) != quote) {
        if (peek(L) == '\\') {
            advance(L); // skip backslash
            if (!at_end(L)) advance(L); // skip escaped char
        } else {
            advance(L);
        }
    }
    if (at_end(L)) {
        return error_token("Unterminated string", L->line, L->col);
    }
    size_t len = L->pos - start_pos;
    const char *start = L->src + start_pos;
    advance(L); // consume closing quote
    Token t = make_token(quote=='"' ? TK_STRING : TK_CHAR, start, len, L->line, start_col);
    return t;
}

Token next_token(Lexer *L) {
    skip_whitespace(L);
    if (at_end(L)) return make_token(TK_EOF, "", 0, L->line, L->col);

    char c = peek(L);
    int token_line = L->line;
    int token_col = L->col;

    // identifiers or keywords
    if (isalpha((unsigned char)c) || c == '_') {
        return scan_identifier_or_keyword(L);
    }

    // numbers
    if (isdigit((unsigned char)c)) {
        return scan_number(L);
    }

    // strings or char literals
    if (c == '"' || c == '\'') {
        return scan_string(L);
    }

    // single or two-char tokens
    advance(L); // consume c
    switch (c) {
        case '+': return make_token(TK_PLUS, "+", 1, token_line, token_col);
        case '-': return make_token(TK_MINUS, "-", 1, token_line, token_col);
        case '*': return make_token(TK_STAR, "*", 1, token_line, token_col);
        case '/': return make_token(TK_SLASH, "/", 1, token_line, token_col);
        case ';': return make_token(TK_SEMI, ";", 1, token_line, token_col);
        case ',': return make_token(TK_COMMA, ",", 1, token_line, token_col);
        case '(' : return make_token(TK_LPAREN, "(", 1, token_line, token_col);
        case ')' : return make_token(TK_RPAREN, ")", 1, token_line, token_col);
        case '{' : return make_token(TK_LBRACE, "{", 1, token_line, token_col);
        case '}' : return make_token(TK_RBRACE, "}", 1, token_line, token_col);
        case '!':
            if (peek(L) == '=') { advance(L); return make_token(TK_NEQ, "!=", 2, token_line, token_col); }
            return make_token(TK_NOT, "!", 1, token_line, token_col);
        case '=':
            if (peek(L) == '=') { advance(L); return make_token(TK_EQEQ, "==", 2, token_line, token_col); }
            return make_token(TK_ASSIGN, "=", 1, token_line, token_col);
        case '<':
            if (peek(L) == '=') { advance(L); return make_token(TK_LTE, "<=", 2, token_line, token_col); }
            return make_token(TK_LT, "<", 1, token_line, token_col);
        case '>':
            if (peek(L) == '=') { advance(L); return make_token(TK_GTE, ">=", 2, token_line, token_col); }
            return make_token(TK_GT, ">", 1, token_line, token_col);
        case '&':
            if (peek(L) == '&') { advance(L); return make_token(TK_AND, "&&", 2, token_line, token_col); }
            break;
        case '|':
            if (peek(L) == '|') { advance(L); return make_token(TK_OR, "||", 2, token_line, token_col); }
            break;
        default:
            break;
    }

    // unknown single-char token
    char buf[2] = {c, '\0'};
    return make_token(TK_UNKNOWN, buf, 1, token_line, token_col);
}

// helpers for freeing tokens
void free_token(Token *t) {
    if (t->lexeme) free(t->lexeme);
}

// demo main
int main(int argc, char **argv) {
    const char *sample = "int add(int a, int b) {\n"
                         "  // add two numbers\n"
                         "  return a + b;\n"
                         "}\n"
                         "if (a == 10 && b != 0) { puts(\"ok\"); }\n";

    const char *src = sample;
    if (argc > 1) {
        // read file
        FILE *f = fopen(argv[1], "rb");
        if (!f) { perror("fopen"); return 1; }
        fseek(f, 0, SEEK_END);
        long sz = ftell(f);
        fseek(f, 0, SEEK_SET);
        char *buf = malloc(sz + 1);
        fread(buf, 1, sz, f);
        buf[sz] = '\0';
        fclose(f);
        src = buf;
    }

    Lexer L = lexer_init(src);
    for (;;) {
        Token t = next_token(&L);
        if (t.type == TK_EOF) {
            printf("[EOF]\n");
            free_token(&t);
            break;
        }
        printf("Token: type=%d, lexeme='%s', line=%d col=%d\n", t.type, t.lexeme, t.line, t.column);
        free_token(&t);
    }

    if (argc > 1) free((void*)src);
    return 0;
}
