// mini_js.c - Tokenizer + Pratt parser skeleton for JS-like syntax (C99)
// build: gcc -std=c99 -Wall -Wextra -O2 mini_js.c -o mini_js
// run:   ./mini_js examples/test.js
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <ctype.h>

/* ===================== Utilities ===================== */
typedef struct {
    const char *data;
    size_t len;
} StringView;

static StringView sv_make(const char *s, size_t n){ StringView v={s,n}; return v; }
static int sv_eq(StringView a, const char *b){
    size_t n = strlen(b);
    return a.len==n && (n==0 || memcmp(a.data,b,n)==0);
}

static void* xmalloc(size_t n){ void *p = malloc(n); if(!p){ fprintf(stderr,"oom\n"); exit(1);} return p; }
static void* xrealloc(void *p, size_t n){ p = realloc(p,n); if(!p){ fprintf(stderr,"oom\n"); exit(1);} return p; }

/* ===================== Lexer ===================== */
typedef enum {
    // Special
    T_EOF=0, T_ERROR,
    // Literals/ident
    T_IDENT, T_NUMBER, T_STRING,
    // Keywords
    K_LET, K_CONST, K_VAR, K_FUNCTION, K_RETURN, K_IF, K_ELSE, K_WHILE, K_FOR,
    K_TRUE, K_FALSE, K_NULL, K_UNDEFINED,
    // Punctuators / operators
    T_LPAREN, T_RPAREN, T_LBRACE, T_RBRACE, T_LBRACK, T_RBRACK,
    T_SEMI, T_COMMA, T_DOT, T_COLON, T_QMARK,
    T_PLUS, T_MINUS, T_STAR, T_SLASH, T_PERCENT,
    T_BANG, T_TILDE,
    T_EQ, T_EQEQ, T_NEQ, T_LT, T_LTE, T_GT, T_GTE,
    T_ANDAND, T_OROR,
    T_PLUS_EQ, T_MINUS_EQ, T_STAR_EQ, T_SLASH_EQ, T_PERCENT_EQ,
    T_ARROW, // =>
} TokenKind;

typedef struct {
    TokenKind kind;
    StringView lex; // for identifiers/strings, also for debugging
    double num;     // for number literal
    int line, col;
} Token;

typedef struct {
    const char *src;
    size_t len, pos;
    int line, col;
} Lexer;

static int lx_peek(Lexer *L){ return (L->pos<L->len)? (unsigned char)L->src[L->pos] : 0; }
static int lx_next(Lexer *L){
    if(L->pos>=L->len) return 0;
    int c = (unsigned char)L->src[L->pos++];
    if(c=='\n'){ L->line++; L->col=1; } else { L->col++; }
    return c;
}
static int lx_match(Lexer *L, int c){
    if(lx_peek(L)==c){ lx_next(L); return 1; } return 0;
}
static void skip_ws_and_comments(Lexer *L){
    for(;;){
        int c = lx_peek(L);
        if(c==' '||c=='\t'||c=='\r'||c=='\n'){ lx_next(L); continue; }
        if(c=='/'){
            if(L->pos+1<L->len && L->src[L->pos+1]=='/'){ // line comment
                while((c=lx_next(L)) && c!='\n');
                continue;
            }
            if(L->pos+1<L->len && L->src[L->pos+1]=='*'){ // block comment
                lx_next(L); lx_next(L);
                int prev=0; int cur;
                while((cur=lx_next(L))){
                    if(prev=='*' && cur=='/') break;
                    prev=cur;
                }
                continue;
            }
        }
        break;
    }
}

static Token make_tok(TokenKind k, Lexer *L, const char *start, const char *end){
    Token t; t.kind=k; t.line=L->line; t.col=L->col; t.lex=sv_make(start, (size_t)(end-start)); t.num=0.0; return t;
}
static Token err_tok(Lexer *L, const char *msg){
    Token t; t.kind=T_ERROR; t.line=L->line; t.col=L->col; t.lex=sv_make(msg, strlen(msg)); t.num=0.0; return t;
}

static TokenKind kw_lookup(StringView sv){
    if(sv_eq(sv,"let")) return K_LET;
    if(sv_eq(sv,"const")) return K_CONST;
    if(sv_eq(sv,"var")) return K_VAR;
    if(sv_eq(sv,"function")) return K_FUNCTION;
    if(sv_eq(sv,"return")) return K_RETURN;
    if(sv_eq(sv,"if")) return K_IF;
    if(sv_eq(sv,"else")) return K_ELSE;
    if(sv_eq(sv,"while")) return K_WHILE;
    if(sv_eq(sv,"for")) return K_FOR;
    if(sv_eq(sv,"true")) return K_TRUE;
    if(sv_eq(sv,"false")) return K_FALSE;
    if(sv_eq(sv,"null")) return K_NULL;
    if(sv_eq(sv,"undefined")) return K_UNDEFINED;
    return T_IDENT;
}

static Token lex_string(Lexer *L){
    int q = lx_next(L); // consume quote
    const char *start = &L->src[L->pos];
    while(1){
        int c = lx_peek(L);
        if(c==0) return err_tok(L,"unterminated string");
        if(c=='\\'){ lx_next(L); if(lx_peek(L)) lx_next(L); continue; }
        if(c==q){ const char *end=&L->src[L->pos]; lx_next(L); Token t=make_tok(T_STRING,L,start,end); return t; }
        lx_next(L);
    }
}

static Token lex_number(Lexer *L){
    const char *start = &L->src[L->pos];
    int c=lx_peek(L);
    int saw_dot=0;
    while(isdigit(c) || (!saw_dot && c=='.')){
        if(c=='.') saw_dot=1;
        lx_next(L); c=lx_peek(L);
    }
    const char *end = &L->src[L->pos];
    Token t=make_tok(T_NUMBER,L,start,end);
    char buf[128]; size_t n=(size_t)(end-start);
    n = n<sizeof(buf)-1 ? n : sizeof(buf)-1;
    memcpy(buf,start,n); buf[n]=0;
    t.num = strtod(buf, NULL);
    return t;
}

static Token lex_ident_or_kw(Lexer *L){
    const char *start = &L->src[L->pos];
    int c = lx_peek(L);
    while(isalnum(c) || c=='_' || c=='$'){
        lx_next(L); c=lx_peek(L);
    }
    const char *end = &L->src[L->pos];
    Token t = make_tok(T_IDENT,L,start,end);
    t.kind = kw_lookup(t.lex);
    return t;
}

static Token lex_punct(Lexer *L){
    int c = lx_peek(L);
    switch(c){
        case '(': lx_next(L); return make_tok(T_LPAREN,L,&L->src[L->pos-1],&L->src[L->pos]);
        case ')': lx_next(L); return make_tok(T_RPAREN,L,&L->src[L->pos-1],&L->src[L->pos]);
        case '{': lx_next(L); return make_tok(T_LBRACE,L,&L->src[L->pos-1],&L->src[L->pos]);
        case '}': lx_next(L); return make_tok(T_RBRACE,L,&L->src[L->pos-1],&L->src[L->pos]);
        case '[': lx_next(L); return make_tok(T_LBRACK,L,&L->src[L->pos-1],&L->src[L->pos]);
        case ']': lx_next(L); return make_tok(T_RBRACK,L,&L->src[L->pos-1],&L->src[L->pos]);
        case ';': lx_next(L); return make_tok(T_SEMI,L,&L->src[L->pos-1],&L->src[L->pos]);
        case ',': lx_next(L); return make_tok(T_COMMA,L,&L->src[L->pos-1],&L->src[L->pos]);
        case '.': lx_next(L); return make_tok(T_DOT,L,&L->src[L->pos-1],&L->src[L->pos]);
        case ':': lx_next(L); return make_tok(T_COLON,L,&L->src[L->pos-1],&L->src[L->pos]);
        case '?': lx_next(L); return make_tok(T_QMARK,L,&L->src[L->pos-1],&L->src[L->pos]);
        case '~': lx_next(L); return make_tok(T_TILDE,L,&L->src[L->pos-1],&L->src[L->pos]);
        case '!':
            lx_next(L);
            if(lx_match(L,'=')) return make_tok(T_NEQ,L,&L->src[L->pos-2],&L->src[L->pos]);
            return make_tok(T_BANG,L,&L->src[L->pos-1],&L->src[L->pos]);
        case '+':
            lx_next(L);
            if(lx_match(L,'=')) return make_tok(T_PLUS_EQ,L,&L->src[L->pos-2],&L->src[L->pos]);
            return make_tok(T_PLUS,L,&L->src[L->pos-1],&L->src[L->pos]);
        case '-':
            // arrow =>
            if(L->pos+1<L->len && L->src[L->pos+1]=='>'){ lx_next(L); lx_next(L); return make_tok(T_ARROW,L,&L->src[L->pos-2],&L->src[L->pos]); }
            lx_next(L);
            if(lx_match(L,'=')) return make_tok(T_MINUS_EQ,L,&L->src[L->pos-2],&L->src[L->pos]);
            return make_tok(T_MINUS,L,&L->src[L->pos-1],&L->src[L->pos]);
        case '*':
            lx_next(L);
            if(lx_match(L,'=')) return make_tok(T_STAR_EQ,L,&L->src[L->pos-2],&L->src[L->pos]);
            return make_tok(T_STAR,L,&L->src[L->pos-1],&L->src[L->pos]);
        case '/':
            lx_next(L);
            if(lx_match(L,'=')) return make_tok(T_SLASH_EQ,L,&L->src[L->pos-2],&L->src[L->pos]);
            return make_tok(T_SLASH,L,&L->src[L->pos-1],&L->src[L->pos]);
        case '%':
            lx_next(L);
            if(lx_match(L,'=')) return make_tok(T_PERCENT_EQ,L,&L->src[L->pos-2],&L->src[L->pos]);
            return make_tok(T_PERCENT,L,&L->src[L->pos-1],&L->src[L->pos]);
        case '=':
            lx_next(L);
            if(lx_match(L,'=')) return make_tok(T_EQEQ,L,&L->src[L->pos-2],&L->src[L->pos]);
            return make_tok(T_EQ,L,&L->src[L->pos-1],&L->src[L->pos]);
        case '<':
            lx_next(L);
            if(lx_match(L,'=')) return make_tok(T_LTE,L,&L->src[L->pos-2],&L->src[L->pos]);
            return make_tok(T_LT,L,&L->src[L->pos-1],&L->src[L->pos]);
        case '>':
            lx_next(L);
            if(lx_match(L,'=')) return make_tok(T_GTE,L,&L->src[L->pos-2],&L->src[L->pos]);
            return make_tok(T_GT,L,&L->src[L->pos-1],&L->src[L->pos]);
        case '&':
            lx_next(L);
            if(lx_match(L,'&')) return make_tok(T_ANDAND,L,&L->src[L->pos-2],&L->src[L->pos]);
            break;
        case '|':
            lx_next(L);
            if(lx_match(L,'|')) return make_tok(T_OROR,L,&L->src[L->pos-2],&L->src[L->pos]);
            break;
    }
    return err_tok(L, "unknown punctuator");
}

static Token next_token(Lexer *L){
    skip_ws_and_comments(L);
    if(L->pos>=L->len) { Token t={0}; t.kind=T_EOF; t.line=L->line; t.col=L->col; return t; }
    int c = lx_peek(L);
    if(c=='"' || c=='\'') return lex_string(L);
    if(isdigit(c)) return lex_number(L);
    if(isalpha(c) || c=='_' || c=='$') return lex_ident_or_kw(L);
    return lex_punct(L);
}

/* ===================== AST ===================== */
typedef enum {
    // Expressions
    E_LITERAL, E_IDENT, E_UNARY, E_BINARY, E_ASSIGN,
    E_CALL, E_MEMBER, E_INDEX, E_GROUP, E_FUNC,
    E_TERNARY,
    // Statements
    S_EXPR, S_VAR, S_BLOCK, S_IF, S_WHILE, S_RETURN, S_FUNCDECL,
} NodeKind;

typedef struct Node Node;
typedef struct NodeList { Node **v; size_t n, cap; } NodeList;

typedef struct {
    // for E_LITERAL
    enum { L_NUMBER, L_STRING, L_BOOL, L_NULL, L_UNDEF } lkind;
    double num;
    StringView str;
    int boolean;
} Lit;

typedef struct {
    StringView name;
} Ident;

typedef struct {
    int op; // TokenKind for operator
    Node *expr;
} Unary;

typedef struct {
    int op; // TokenKind
    Node *left, *right;
} Binary;

typedef struct {
    Node *target; // lhs
    int op;       // =, +=, ...
    Node *value;
} Assign;

typedef struct {
    Node *callee;
    NodeList args;
} Call;

typedef struct {
    Node *obj;
    StringView prop;
} Member;

typedef struct {
    Node *obj;
    Node *index;
} Index;

typedef struct {
    Node *cond, *thenE, *elseE;
} Ternary;

typedef struct {
    // function literal or declaration
    StringView name; // empty for anonymous
    NodeList params; // list of Ident nodes
    Node *body;      // S_BLOCK
    int is_arrow;
} Func;

typedef struct {
    Node *expr; // S_EXPR
} StmtExpr;

typedef struct {
    int is_const;
    StringView name;
    Node *init; // nullable
} StmtVar;

typedef struct {
    NodeList stmts;
} StmtBlock;

typedef struct {
    Node *cond;
    Node *thenS;
    Node *elseS; // nullable
} StmtIf;

typedef struct {
    Node *cond;
    Node *body;
} StmtWhile;

typedef struct {
    Node *value; // nullable
} StmtReturn;

typedef struct {
    Func fn; // name filled
} StmtFuncDecl;

struct Node {
    NodeKind kind;
    int line, col;
    union {
        Lit lit;
        Ident ident;
        Unary unary;
        Binary binary;
        Assign assign;
        Call call;
        Member member;
        Index index;
        Ternary ternary;
        Func func;

        StmtExpr sexpr;
        StmtVar svar;
        StmtBlock sblock;
        StmtIf sif;
        StmtWhile swhile;
        StmtReturn sret;
        StmtFuncDecl sfuncdecl;
    } as;
};

static Node* new_node(NodeKind k, int line, int col){
    Node *n = (Node*)xmalloc(sizeof(Node));
    memset(n,0,sizeof(*n));
    n->kind=k; n->line=line; n->col=col; return n;
}
static void nlist_push(NodeList *L, Node *n){
    if(L->n+1>L->cap){ L->cap = L->cap? L->cap*2 : 8; L->v = (Node**)xrealloc(L->v, L->cap*sizeof(Node*)); }
    L->v[L->n++] = n;
}

/* ===================== Parser (Pratt) ===================== */
typedef struct {
    Lexer lex;
    Token cur, peek;
    int had_error;
} Parser;

static void p_next(Parser *P){
    P->cur = P->peek;
    P->peek = next_token(&P->lex);
}
static void p_init(Parser *P, const char *src, size_t len){
    memset(P,0,sizeof(*P));
    P->lex.src=src; P->lex.len=len; P->lex.pos=0; P->lex.line=1; P->lex.col=1;
    P->cur = next_token(&P->lex);
    P->peek = next_token(&P->lex);
}
static void perr(Parser *P, const char *msg){
    fprintf(stderr,"ParseError(%d:%d): %s\n", P->cur.line, P->cur.col, msg);
    P->had_error=1;
}
static int p_accept(Parser *P, TokenKind k){
    if(P->cur.kind==k){ p_next(P); return 1; } return 0;
}
static int p_expect(Parser *P, TokenKind k, const char *msg){
    if(!p_accept(P,k)){ perr(P,msg); return 0; } return 1;
}

/* Precedence levels (higher binds tighter)
   9: call/member/index/postfix
   8: unary (! ~ -)
   7: * / %
   6: + -
   5: < <= > >=
   4: == !=
   3: &&
   2: ||
   1: ternary ?:
   0: assignment (=, +=, ...)
*/
static int precedence(TokenKind k){
    switch(k){
        case T_STAR: case T_SLASH: case T_PERCENT: return 7;
        case T_PLUS: case T_MINUS: return 6;
        case T_LT: case T_LTE: case T_GT: case T_GTE: return 5;
        case T_EQEQ: case T_NEQ: return 4;
        case T_ANDAND: return 3;
        case T_OROR: return 2;
        default: return -1;
    }
}

static int is_assign_op(TokenKind k){
    return k==T_EQ || k==T_PLUS_EQ || k==T_MINUS_EQ || k==T_STAR_EQ || k==T_SLASH_EQ || k==T_PERCENT_EQ;
}

static Node* parse_expression(Parser *P);
static Node* parse_statement(Parser *P);
static Node* parse_block(Parser *P);

static Node* parse_primary(Parser *P){
    Token t = P->cur;
    switch(t.kind){
        case T_NUMBER: {
            Node *n=new_node(E_LITERAL,t.line,t.col);
            n->as.lit.lkind=L_NUMBER; n->as.lit.num=t.num; p_next(P); return n;
        }
        case T_STRING: {
            Node *n=new_node(E_LITERAL,t.line,t.col);
            n->as.lit.lkind=L_STRING; n->as.lit.str=t.lex; p_next(P); return n;
        }
        case K_TRUE: case K_FALSE: {
            Node *n=new_node(E_LITERAL,t.line,t.col);
            n->as.lit.lkind=L_BOOL; n->as.lit.boolean=(t.kind==K_TRUE); p_next(P); return n;
        }
        case K_NULL: {
            Node *n=new_node(E_LITERAL,t.line,t.col);
            n->as.lit.lkind=L_NULL; p_next(P); return n;
        }
        case K_UNDEFINED: {
            Node *n=new_node(E_LITERAL,t.line,t.col);
            n->as.lit.lkind=L_UNDEF; p_next(P); return n;
        }
        case T_IDENT: {
            Node *n=new_node(E_IDENT,t.line,t.col);
            n->as.ident.name=t.lex; p_next(P); return n;
        }
        case T_LPAREN: {
            p_next(P);
            Node *e = parse_expression(P);
            p_expect(P,T_RPAREN,") expected");
            Node *n=new_node(E_GROUP,t.line,t.col);
            n->as.unary.expr=e;
            return n;
        }
        case K_FUNCTION: {
            // function name?(params){block}
            p_next(P);
            Token name = P->cur;
            StringView nm = sv_make("",0);
            if(name.kind==T_IDENT){ nm=name.lex; p_next(P); }
            if(!p_expect(P,T_LPAREN,"( expected after function")) return NULL;
            NodeList params={0};
            if(P->cur.kind!=T_RPAREN){
                do{
                    if(P->cur.kind!=T_IDENT){ perr(P,"parameter name expected"); break; }
                    Node *id=new_node(E_IDENT,P->cur.line,P->cur.col);
                    id->as.ident.name=P->cur.lex;
                    nlist_push(&params,id);
                    p_next(P);
                } while(p_accept(P,T_COMMA));
            }
            p_expect(P,T_RPAREN,") expected after params");
            Node *body = parse_block(P);
            Node *fn = new_node(E_FUNC,t.line,t.col);
            fn->as.func.name=nm;
            fn->as.func.params=params;
            fn->as.func.body=body;
            fn->as.func.is_arrow=0;
            return fn;
        }
        default:
            perr(P,"primary expression expected");
            return NULL;
    }
}

static Node* parse_postfix(Parser *P){
    Node *expr = parse_primary(P);
    if(!expr) return NULL;
    for(;;){
        if(P->cur.kind==T_LPAREN){
            // call
            Token t=P->cur; p_next(P);
            NodeList args={0};
            if(P->cur.kind!=T_RPAREN){
                do{
                    Node *a = parse_expression(P);
                    if(!a) break;
                    nlist_push(&args,a);
                } while(p_accept(P,T_COMMA));
            }
            p_expect(P,T_RPAREN,") expected after args");
            Node *call=new_node(E_CALL,t.line,t.col);
            call->as.call.callee=expr;
            call->as.call.args=args;
            expr=call;
            continue;
        }
        if(P->cur.kind==T_DOT){
            p_next(P);
            if(P->cur.kind!=T_IDENT){ perr(P,"property name expected after ."); break; }
            Node *m=new_node(E_MEMBER,P->cur.line,P->cur.col);
            m->as.member.obj=expr;
            m->as.member.prop=P->cur.lex;
            p_next(P);
            expr=m;
            continue;
        }
        if(P->cur.kind==T_LBRACK){
            Token t=P->cur; p_next(P);
            Node *idx = parse_expression(P);
            p_expect(P,T_RBRACK,"] expected");
            Node *ix=new_node(E_INDEX,t.line,t.col);
            ix->as.index.obj=expr; ix->as.index.index=idx;
            expr=ix;
            continue;
        }
        break;
    }
    return expr;
}

static Node* parse_unary(Parser *P){
    if(P->cur.kind==T_BANG || P->cur.kind==T_MINUS || P->cur.kind==T_TILDE){
        Token t=P->cur; p_next(P);
        Node *e = parse_unary(P);
        Node *n=new_node(E_UNARY,t.line,t.col);
        n->as.unary.op=t.kind; n->as.unary.expr=e; return n;
    }
    return parse_postfix(P);
}

static Node* parse_ternary(Parser *P, Node *cond){
    // cond ? a : b
    Node *n = new_node(E_TERNARY, P->cur.line, P->cur.col);
    p_next(P); // consume ?
    Node *a = parse_expression(P);
    p_expect(P,T_COLON,": expected in ternary");
    Node *b = parse_expression(P);
    n->as.ternary.cond=cond; n->as.ternary.thenE=a; n->as.ternary.elseE=b;
    return n;
}

static Node* parse_binary_rhs(Parser *P, int min_prec, Node *lhs){
    for(;;){
        TokenKind op = P->cur.kind;
        if(op==T_QMARK){ // ternary binds between || and assignment here
            if(min_prec <= 1){ // ternary precedence 1
                lhs = parse_ternary(P, lhs);
                continue;
            } else break;
        }
        int prec = precedence(op);
        if(prec < min_prec) break;
        Token t = P->cur; p_next(P);
        Node *rhs = parse_unary(P);
        // precedence climbing (left-assoc)
        while(1){
            int next_prec = precedence(P->cur.kind);
            if(next_prec > prec){
                rhs = parse_binary_rhs(P, next_prec, rhs);
            } else break;
        }
        Node *bin=new_node(E_BINARY,t.line,t.col);
        bin->as.binary.op=op; bin->as.binary.left=lhs; bin->as.binary.right=rhs;
        lhs=bin;
    }
    return lhs;
}

static int is_lvalue(Node *n){
    return n && (n->kind==E_IDENT || n->kind==E_MEMBER || n->kind==E_INDEX);
}

static Node* parse_assignment(Parser *P){
    Node *lhs = parse_unary(P);
    if(!lhs) return NULL;
    if(is_assign_op(P->cur.kind)){
        Token t=P->cur; p_next(P);
        Node *val = parse_expression(P);
        if(!is_lvalue(lhs)) perr(P,"left-hand side of assignment must be assignable");
        Node *n=new_node(E_ASSIGN,t.line,t.col);
        n->as.assign.target=lhs; n->as.assign.op=t.kind; n->as.assign.value=val;
        return n;
    }
    // otherwise parse binary/ternary with lhs as start
    return parse_binary_rhs(P, 2, lhs); // min_prec: start above || for ternary handled separately
}

static Node* parse_expression(Parser *P){
    return parse_assignment(P);
}

/* ----- Statements ----- */

static Node* parse_var_decl(Parser *P, int is_const){
    // let|const|var name (= init)? (; optional)
    Token name = P->cur;
    if(name.kind!=T_IDENT){ perr(P,"variable name expected"); return NULL; }
    p_next(P);
    Node *init=NULL;
    if(p_accept(P,T_EQ)){
        init = parse_expression(P);
    }
    Node *n=new_node(S_VAR,name.line,name.col);
    n->as.svar.is_const=is_const;
    n->as.svar.name = name.lex;
    n->as.svar.init = init;
    if(P->cur.kind==T_SEMI) p_next(P);
    return n;
}

static Node* parse_if(Parser *P){
    // if (cond) stmt else stmt?
    Token t=P->cur; p_next(P);
    p_expect(P,T_LPAREN,"( expected after if");
    Node *cond = parse_expression(P);
    p_expect(P,T_RPAREN,") expected after condition");
    Node *thenS = parse_statement(P);
    Node *elseS=NULL;
    if(p_accept(P,K_ELSE)) elseS = parse_statement(P);
    Node *n=new_node(S_IF,t.line,t.col);
    n->as.sif.cond=cond; n->as.sif.thenS=thenS; n->as.sif.elseS=elseS;
    return n;
}

static Node* parse_while(Parser *P){
    // while (cond) stmt
    Token t=P->cur; p_next(P);
    p_expect(P,T_LPAREN,"( expected after while");
    Node *cond = parse_expression(P);
    p_expect(P,T_RPAREN,") expected after condition");
    Node *body = parse_statement(P);
    Node *n=new_node(S_WHILE,t.line,t.col);
    n->as.swhile.cond=cond; n->as.swhile.body=body;
    return n;
}

static Node* parse_return(Parser *P){
    Token t=P->cur; p_next(P);
    Node *val=NULL;
    if(P->cur.kind!=T_SEMI && P->cur.kind!=T_RBRACE && P->cur.kind!=T_EOF){
        val = parse_expression(P);
    }
    if(P->cur.kind==T_SEMI) p_next(P);
    Node *n=new_node(S_RETURN,t.line,t.col);
    n->as.sret.value=val; return n;
}

static Node* parse_func_decl(Parser *P){
    // function name(params){block}
    Token kw=P->cur; p_next(P);
    Token name=P->cur;
    if(name.kind!=T_IDENT){ perr(P,"function name expected"); return NULL; }
    p_next(P);
    p_expect(P,T_LPAREN,"( expected after function name");
    NodeList params={0};
    if(P->cur.kind!=T_RPAREN){
        do{
            if(P->cur.kind!=T_IDENT){ perr(P,"parameter name expected"); break; }
            Node *id=new_node(E_IDENT,P->cur.line,P->cur.col);
            id->as.ident.name=P->cur.lex;
            nlist_push(&params,id);
            p_next(P);
        } while(p_accept(P,T_COMMA));
    }
    p_expect(P,T_RPAREN,") expected after params");
    Node *body = parse_block(P);
    Node *n=new_node(S_FUNCDECL,kw.line,kw.col);
    n->as.sfuncdecl.fn.name=name.lex;
    n->as.sfuncdecl.fn.params=params;
    n->as.sfuncdecl.fn.body=body;
    n->as.sfuncdecl.fn.is_arrow=0;
    return n;
}

static Node* parse_block(Parser *P){
    // { stmt* }
    Token t=P->cur; p_next(P);
    NodeList list={0};
    while(P->cur.kind!=T_RBRACE && P->cur.kind!=T_EOF){
        Node *s = parse_statement(P);
        if(!s) break;
        nlist_push(&list,s);
    }
    p_expect(P,T_RBRACE,"} expected to close block");
    Node *n=new_node(S_BLOCK,t.line,t.col);
    n->as.sblock.stmts=list;
    return n;
}

static Node* parse_statement(Parser *P){
    switch(P->cur.kind){
        case T_SEMI: p_next(P); { Node *n=new_node(S_EXPR,P->cur.line,P->cur.col); n->as.sexpr.expr=NULL; return n; }
        case T_LBRACE: return parse_block(P);
        case K_IF: return parse_if(P);
        case K_WHILE: return parse_while(P);
        case K_RETURN: return parse_return(P);
        case K_FUNCTION: return parse_func_decl(P);
        case K_LET: p_next(P); return parse_var_decl(P,0);
        case K_CONST: p_next(P); return parse_var_decl(P,1);
        case K_VAR: p_next(P); return parse_var_decl(P,0);
        default: {
            Node *e = parse_expression(P);
            Node *n=new_node(S_EXPR, P->cur.line, P->cur.col);
            n->as.sexpr.expr=e;
            if(P->cur.kind==T_SEMI) p_next(P);
            return n;
        }
    }
}

/* Program = stmt* EOF */
static Node* parse_program(Parser *P){
    Node *root = new_node(S_BLOCK, 1,1);
    NodeList list={0};
    while(P->cur.kind!=T_EOF && !P->had_error){
        Node *s = parse_statement(P);
        if(!s) break;
        nlist_push(&list,s);
    }
    root->as.sblock.stmts=list;
    return root;
}

/* ===================== AST Printer (debug) ===================== */
static void print_indent(int d){ for(int i=0;i<d;i++) printf("  "); }

static void print_sv(StringView sv){
    for(size_t i=0;i<sv.len;i++){ putchar(sv.data[i]); }
}

static void print_node(Node *n, int d);

static void print_list(const char *label, NodeList *L, int d){
    print_indent(d); printf("%s [%zu]\n", label, L->n);
    for(size_t i=0;i<L->n;i++) print_node(L->v[i], d+1);
}

static const char* op_name(int k){
    switch(k){
        case T_PLUS: return "+"; case T_MINUS: return "-"; case T_STAR: return "*";
        case T_SLASH: return "/"; case T_PERCENT: return "%%";
        case T_EQEQ: return "=="; case T_NEQ: return "!=";
        case T_LT: return "<"; case T_LTE: return "<="; case T_GT: return ">"; case T_GTE: return ">=";
        case T_ANDAND: return "&&"; case T_OROR: return "||";
        case T_BANG: return "!"; case T_TILDE: return "~";
        case T_EQ: return "="; case T_PLUS_EQ: return "+="; case T_MINUS_EQ: return "-=";
        case T_STAR_EQ: return "*="; case T_SLASH_EQ: return "/="; case T_PERCENT_EQ: return "%%=";
        default: return "?";
    }
}

static void print_node(Node *n, int d){
    if(!n){ print_indent(d); printf("(null)\n"); return; }
    switch(n->kind){
        case E_LITERAL:
            print_indent(d); printf("Literal ");
            switch(n->as.lit.lkind){
                case L_NUMBER: printf("%g\n", n->as.lit.num); break;
                case L_STRING: printf("\""); print_sv(n->as.lit.str); printf("\"\n"); break;
                case L_BOOL: printf(n->as.lit.boolean?"true\n":"false\n"); break;
                case L_NULL: printf("null\n"); break;
                case L_UNDEF: printf("undefined\n"); break;
            } break;
        case E_IDENT:
            print_indent(d); printf("Ident "); print_sv(n->as.ident.name); printf("\n"); break;
        case E_GROUP:
            print_indent(d); printf("Group\n"); print_node(n->as.unary.expr,d+1); break;
        case E_UNARY:
            print_indent(d); printf("Unary %s\n", op_name(n->as.unary.op));
            print_node(n->as.unary.expr,d+1); break;
        case E_BINARY:
            print_indent(d); printf("Binary %s\n", op_name(n->as.binary.op));
            print_node(n->as.binary.left,d+1);
            print_node(n->as.binary.right,d+1);
            break;
        case E_ASSIGN:
            print_indent(d); printf("Assign %s\n", op_name(n->as.assign.op));
            print_node(n->as.assign.target,d+1);
            print_node(n->as.assign.value,d+1);
            break;
        case E_CALL:
            print_indent(d); printf("Call\n");
            print_node(n->as.call.callee,d+1);
            print_list("args",&n->as.call.args,d+1);
            break;
        case E_MEMBER:
            print_indent(d); printf("Member ."); print_sv(n->as.member.prop); printf("\n");
            print_node(n->as.member.obj,d+1);
            break;
        case E_INDEX:
            print_indent(d); printf("Index\n");
            print_node(n->as.index.obj,d+1);
            print_node(n->as.index.index,d+1);
            break;
        case E_TERNARY:
            print_indent(d); printf("Ternary\n");
            print_node(n->as.ternary.cond,d+1);
            print_node(n->as.ternary.thenE,d+1);
            print_node(n->as.ternary.elseE,d+1);
            break;
        case E_FUNC: {
            print_indent(d); printf("Func %s%s\n", n->as.func.is_arrow?"arrow ":"",
                   n->as.func.name.len? "named":"anonymous");
            if(n->as.func.name.len){ print_indent(d+1); printf("name "); print_sv(n->as.func.name); printf("\n"); }
            print_list("params",&n->as.func.params,d+1);
            print_node(n->as.func.body,d+1);
        } break;
        case S_EXPR:
            print_indent(d); printf("StmtExpr\n"); print_node(n->as.sexpr.expr,d+1); break;
        case S_VAR:
            print_indent(d); printf("Var %s ", n->as.svar.is_const?"const":"let/var");
            print_sv(n->as.svar.name); printf("\n");
            if(n->as.svar.init){ print_indent(d+1); printf("init:\n"); print_node(n->as.svar.init,d+2); }
            break;
        case S_BLOCK:
            print_list("Block",&n->as.sblock.stmts,d); break;
        case S_IF:
            print_indent(d); printf("If\n");
            print_indent(d+1); printf("cond:\n"); print_node(n->as.sif.cond,d+2);
            print_indent(d+1); printf("then:\n"); print_node(n->as.sif.thenS,d+2);
            if(n->as.sif.elseS){ print_indent(d+1); printf("else:\n"); print_node(n->as.sif.elseS,d+2); }
            break;
        case S_WHILE:
            print_indent(d); printf("While\n");
            print_node(n->as.swhile.cond,d+1);
            print_node(n->as.swhile.body,d+1);
            break;
        case S_RETURN:
            print_indent(d); printf("Return\n");
            print_node(n->as.sret.value,d+1);
            break;
        case S_FUNCDECL:
            print_indent(d); printf("FuncDecl\n");
            print_indent(d+1); printf("name "); print_sv(n->as.sfuncdecl.fn.name); printf("\n");
            print_list("params",&n->as.sfuncdecl.fn.params,d+1);
            print_node(n->as.sfuncdecl.fn.body,d+1);
            break;
        default:
            print_indent(d); printf("UnknownNode\n");
    }
}

/* ===================== Main ===================== */
static char* read_file(const char *path, size_t *out_len){
    FILE *f = fopen(path,"rb");
    if(!f){ perror("fopen"); return NULL; }
    fseek(f,0,SEEK_END);
    long n = ftell(f);
    fseek(f,0,SEEK_SET);
    char *buf = (char*)xmalloc((size_t)n+1);
    if(fread(buf,1,(size_t)n,f)!=(size_t)n){ perror("fread"); fclose(f); free(buf); return NULL; }
    fclose(f); buf[n]=0; if(out_len) *out_len=(size_t)n; return buf;
}

int main(int argc, char **argv){
    if(argc<2){
        fprintf(stderr,"usage: %s <source.js>\n", argv[0]);
        return 2;
    }
    size_t len=0;
    char *src = read_file(argv[1], &len);
    if(!src) return 1;

    Parser P; p_init(&P, src, len);
    Node *prog = parse_program(&P);
    if(!P.had_error){
        print_node(prog, 0);
    } else {
        fprintf(stderr,"Parsing failed.\n");
        free(src);
        return 1;
    }
    free(src);
    return 0;
}
