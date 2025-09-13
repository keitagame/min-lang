// tinyvm.c - minimal stack VM (C99)
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef enum {
  T_NIL, T_INT, T_BOOL
} Tag;

typedef struct {
  Tag tag;
  union { int64_t i; int b; } as;
} Value;

static inline Value VInt(int64_t x){ return (Value){T_INT, .as.i=x}; }
static inline Value VBool(int b){ return (Value){T_BOOL, .as.b=b!=0}; }
static inline Value VNil(void){ return (Value){T_NIL,{0}}; }

typedef enum {
  OP_PUSH_INT,   // imm: int64
  OP_ADD,
  OP_SUB,
  OP_MUL,
  OP_DIV,
  OP_EQ,
  OP_LT,
  OP_JMP,        // imm: int32 (relative)
  OP_JMPF,       // imm: int32 (relative) if false
  OP_CALL,       // imm: argc (u8), func_id (u8)
  OP_RET
} Op;

typedef struct {
  uint8_t *code;
  size_t len, cap;
  int64_t *consts;
  size_t clen, ccap;
} Chunk;

static void emit_u8(Chunk *c, uint8_t b){
  if(c->len+1>c->cap){ c->cap=c->cap?c->cap*2:128; c->code=realloc(c->code,c->cap); }
  c->code[c->len++]=b;
}
static void emit_u32(Chunk *c, uint32_t v){ for(int i=0;i<4;i++) emit_u8(c,(v>>(i*8))&0xFF); }
static void emit_i64(Chunk *c, int64_t v){ for(int i=0;i<8;i++) emit_u8(c,(uint8_t)((uint64_t)v>>(i*8))); }

static uint32_t add_const(Chunk *c, int64_t v){
  if(c->clen+1>c->ccap){ c->ccap=c->ccap?c->ccap*2:64; c->consts=realloc(c->consts,sizeof(int64_t)*c->ccap); }
  c->consts[c->clen]=v; return (uint32_t)c->clen++;
}

typedef struct {
  Value *stack; size_t sp, cap;
  const uint8_t *ip;
} VM;

static void push(VM*vm, Value v){
  if(vm->sp+1>vm->cap){ vm->cap=vm->cap?vm->cap*2:256; vm->stack=realloc(vm->stack, vm->cap*sizeof(Value)); }
  vm->stack[vm->sp++]=v;
}
static Value pop(VM*vm){ return vm->stack[--vm->sp]; }

typedef Value (*Native)(Value *args, uint8_t argc);

static Value native_print(Value *args, uint8_t argc){
  for(uint8_t i=0;i<argc;i++){
    Value v=args[i];
    if(v.tag==T_INT) printf("%lld", (long long)v.as.i);
    else if(v.tag==T_BOOL) printf(v.as.b?"true":"false");
    else printf("nil");
    if(i+1<argc) printf(" ");
  }
  printf("\n");
  return VNil();
}

static Native natives[256];

static int run(VM*vm, const Chunk *c){
  vm->ip = c->code;
  for(;;){
    Op op = (Op)*vm->ip++;
    switch(op){
      case OP_PUSH_INT: {
        uint32_t idx = 0; // read u32 const index
        for(int i=0;i<4;i++) idx |= ((uint32_t)vm->ip[i])<<(i*8);
        vm->ip += 4;
        push(vm, VInt(c->consts[idx]));
      } break;
      case OP_ADD: { Value b=pop(vm), a=pop(vm); push(vm, VInt(a.as.i + b.as.i)); } break;
      case OP_SUB: { Value b=pop(vm), a=pop(vm); push(vm, VInt(a.as.i - b.as.i)); } break;
      case OP_MUL: { Value b=pop(vm), a=pop(vm); push(vm, VInt(a.as.i * b.as.i)); } break;
      case OP_DIV: { Value b=pop(vm), a=pop(vm); push(vm, VInt(a.as.i / b.as.i)); } break;
      case OP_EQ:  { Value b=pop(vm), a=pop(vm); push(vm, VBool(a.as.i==b.as.i)); } break;
      case OP_LT:  { Value b=pop(vm), a=pop(vm); push(vm, VBool(a.as.i<b.as.i)); } break;
      case OP_JMP: { int32_t rel=0; for(int i=0;i<4;i++) rel |= ((int32_t)vm->ip[i])<<(i*8); vm->ip+=4; vm->ip += rel; } break;
      case OP_JMPF: {
        int32_t rel=0; for(int i=0;i<4;i++) rel |= ((int32_t)vm->ip[i])<<(i*8);
        vm->ip+=4;
        Value cond = pop(vm);
        if(!(cond.tag==T_BOOL && cond.as.b)) vm->ip += rel;
      } break;
      case OP_CALL: {
        uint8_t argc = *vm->ip++;
        uint8_t id   = *vm->ip++;
        Value *args = &vm->stack[vm->sp-argc];
        Value ret = natives[id](args, argc);
        vm->sp -= argc;
        push(vm, ret);
      } break;
      case OP_RET:
        return 0;
      default:
        fprintf(stderr,"bad opcode %d\n", op); return 1;
    }
  }
}

static void emit_push_const(Chunk*c, int64_t v){
  uint32_t idx = add_const(c, v);
  emit_u8(c, OP_PUSH_INT); emit_u32(c, idx);
}

int main(void){
  natives[0] = native_print;

  Chunk c = {0};
  // program: print( (1+2)*3 == 9 )
  emit_push_const(&c, 1);
  emit_push_const(&c, 2);
  emit_u8(&c, OP_ADD);
  emit_push_const(&c, 3);
  emit_u8(&c, OP_MUL);
  emit_push_const(&c, 9);
  emit_u8(&c, OP_EQ);
  emit_u8(&c, OP_CALL); emit_u8(&c, 1); emit_u8(&c, 0); // argc=1, func_id=0
  emit_u8(&c, OP_RET);

  VM vm = {0};
  int rc = run(&vm, &c);
  free(vm.stack); free(c.code); free(c.consts);
  return rc;
}
