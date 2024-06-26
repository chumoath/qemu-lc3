#include "qemu/osdep.h"
#include "cpu.h"

typedef struct {
    disassemble_info *info;
    uint16_t next_word;
    bool next_word_used;
} DisasContext;

static bool decode_insn(DisasContext *ctx, uint16_t insn);
#include "decode-insn.c.inc"

#define output(mnemonic, format, ...) \
    (pctx->info->fprintf_func(pctx->info->stream, "%-9s " format, \
                              mnemonic, ##__VA_ARGS__))

int lc3_print_insn(bfd_vma addr, disassemble_info *info)
{
    DisasContext ctx;
    DisasContext *pctx = &ctx;
    bfd_byte buffer[4];
    uint16_t insn;
    int status;

    ctx.info = info;

    status = info->read_memory_func(addr, buffer, 4, info);
    if (status != 0) {
        info->memory_error_func(status, addr, info);
        return -1;
    }
    insn = bfd_getl16(buffer);
    ctx.next_word = bfd_getl16(buffer + 2);
    ctx.next_word_used = false;

    if (!decode_insn(&ctx, insn)) {
        output(".db", "0x%02x, 0x%02x", buffer[0], buffer[1]);
    }

    return ctx.next_word_used ? 4 : 2;
}


#define INSN(opcode, format, ...)                                       \
static bool trans_##opcode(DisasContext *pctx, arg_##opcode * a)        \
{                                                                       \
    output(#opcode, format, ##__VA_ARGS__);                             \
    return true;                                                        \
}

static int sign_extend(int x, int bit_count)
{
    if ((x >> (bit_count - 1)) & 1) {
        x |= (0xFFFFFFFF << bit_count);
    }
    return x;
}

INSN(BR,   "n%d z%d p%d PCoffset9%d", a->n, a->z, a->p, sign_extend(a->PCoffset9, 9))
INSN(ADD,  "DR %d SR1 %d SR2 %d", a->DR, a->SR1, a->SR2)
INSN(ADDI, "DR %d SR1 %d imm5 %d", a->DR, a->SR1, sign_extend(a->imm5, 5))
INSN(LD,   "DR %d PCoffset9 %d", a->DR, sign_extend(a->PCoffset9, 9))
INSN(ST,   "SR %d PCoffset9 %d", a->SR, sign_extend(a->PCoffset9, 9))
INSN(JSR,  "PCoffset11 %d", sign_extend(a->PCoffset11, 11))
INSN(JSRR, "BaseR %d", a->BaseR)
INSN(AND,  "DR %d SR1 %d SR2 %d", a->DR, a->SR1, a->SR2)
INSN(ANDI, "DR %d SR1 %d imm5 %d", a->DR, a->SR1, sign_extend(a->imm5, 5))
INSN(LDR,  "DR %d BaseR %d offset6 %d", a->DR, a->BaseR, sign_extend(a->offset6, 6))
INSN(STR,  "SR %d BaseR %d offset6 %d", a->SR, a->BaseR, sign_extend(a->offset6, 6))
INSN(RTI,  "")
INSN(NOT,  "DR %d SR %d", a->DR, a->SR)
INSN(LDI,  "DR %d PCoffset9 %d", a->DR, sign_extend(a->PCoffset9, 9))
INSN(STI,  "SR %d PCoffset9 %d", a->SR, sign_extend(a->PCoffset9, 9))
INSN(JMP,  "BaseR %d", a->BaseR)
INSN(LEA,  "DR %d PCoffset9 %d", a->DR, sign_extend(a->PCoffset9, 9))
INSN(TRAP, "trapvect8 %d", a->trapvect8)