#include "lexer.h"
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

static const char *src;
static size_t pos;

static char peek() { return src[pos]; }
static char advance() { return src[pos++]; }

static Token make_token(TokenType type, const char *start, size_t len) {
    Token t;
    t.type = type;
    t.lexeme = malloc(len + 1);
    memcpy(t.lexeme, start, len);
    t.lexeme[len] = '\0';
    return t;
}

void lexer_init(const char *source) {
    src = source;
    pos = 0;
}

void lexer_free_token(Token t) {
    free(t.lexeme);
}

Token lexer_next_token(void) {
    while (isspace(peek())) advance();

    char c = peek();
    if (c == '\0') return make_token(TOK_EOF, "", 0);

    if (c == '@') { advance(); return make_token(TOK_AT, "@", 1); }
    if (c == ':') { advance(); return make_token(TOK_COLON, ":", 1); }
    if (c == '=') { advance(); return make_token(TOK_ASSIGN, "=", 1); }
    if (c == '.') { advance(); return make_token(TOK_DOT, ".", 1); }
    if (c == '(') { advance(); return make_token(TOK_LPAREN, "(", 1); }
    if (c == ')') { advance(); return make_token(TOK_RPAREN, ")", 1); }
    if (c == ',') { advance(); return make_token(TOK_COMMA, ",", 1); }

    if (isdigit(c)) {
        const char *start = src + pos;
        while (isdigit(peek())) advance();
        if (peek() == 'm' && src[pos+1] == 's') {
            advance(); advance();
            return make_token(TOK_MS, start, (src+pos)-start);
        }
        return make_token(TOK_NUMBER, start, (src+pos)-start);
    }

    if (isalpha(c) || c == '_') {
        const char *start = src + pos;
        while (isalnum(peek()) || peek() == '_') advance();
        return make_token(TOK_IDENTIFIER, start, (src+pos)-start);
    }

    if (c == '"') {
        advance();
        const char *start = src + pos;
        while (peek() != '"' && peek() != '\0') advance();
        Token t = make_token(TOK_STRING, start, (src+pos)-start);
        if (peek() == '"') advance();
        return t;
    }

    advance();
    return make_token(TOK_IDENTIFIER, &c, 1);
}
