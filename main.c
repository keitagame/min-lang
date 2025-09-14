#include <stdio.h>
#include "lexer.h"

int main(void) {
    const char *script =
        "@0ms: const path = \"/home/keita/bg.png\"\n"
        "@100ms: game.background.opacity = 1\n";

    lexer_init(script);

    Token tok;
    do {
        tok = lexer_next_token();
        printf("Token: %-12d '%s'\n", tok.type, tok.lexeme);
        lexer_free_token(tok);
    } while (tok.type != TOK_EOF);

    return 0;
}
