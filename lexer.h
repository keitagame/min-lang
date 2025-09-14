#ifndef LEXER_H
#define LEXER_H

#include <stddef.h>

typedef enum {
    TOK_AT, TOK_NUMBER, TOK_MS, TOK_COLON, TOK_IDENTIFIER,
    TOK_ASSIGN, TOK_DOT, TOK_STRING, TOK_LPAREN, TOK_RPAREN,
    TOK_COMMA, TOK_EOF
} TokenType;

typedef struct {
    TokenType type;
    char *lexeme;
} Token;

void lexer_init(const char *source);
Token lexer_next_token(void);
void lexer_free_token(Token t);

#endif
