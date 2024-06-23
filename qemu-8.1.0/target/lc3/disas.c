/*
 * AVR disassembler
 *
 * Copyright (c) 2019-2020 Richard Henderson <rth@twiddle.net>
 * Copyright (c) 2019-2020 Michael Rolnik <mrolnik@gmail.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "qemu/osdep.h"
#include "cpu.h"

typedef struct {
    disassemble_info *info;
    uint16_t next_word;
    bool next_word_used;
} DisasContext;

static int to_regs_16_31_by_one(DisasContext *ctx, int indx)
{
    return 16 + (indx % 16);
}

static int to_regs_16_23_by_one(DisasContext *ctx, int indx)
{
    return 16 + (indx % 8);
}

static int to_regs_24_30_by_two(DisasContext *ctx, int indx)
{
    return 24 + (indx % 4) * 2;
}

static int to_regs_00_30_by_two(DisasContext *ctx, int indx)
{
    return (indx % 16) * 2;
}

static uint16_t next_word(DisasContext *ctx)
{
    ctx->next_word_used = true;
    return ctx->next_word;
}

static int append_16(DisasContext *ctx, int x)
{
    return x << 16 | next_word(ctx);
}

/* Include the auto-generated decoder.  */
static bool decode_insn(DisasContext *ctx, uint16_t insn);
#include "decode-insn.c.inc"

#define output(mnemonic, format, ...) \
    (pctx->info->fprintf_func(pctx->info->stream, "%-9s " format, \
                              mnemonic, ##__VA_ARGS__))

int avr_print_insn(bfd_vma addr, disassemble_info *info)
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

INSN(BR,   "n%d z%d p%d PCoffset9%d", a->n, a->z, a->p, a->PCoffset9)
INSN(ADD,  "DR %d SR1 %d SR2 %d", a->DR, a->SR1, a->SR2)
INSN(ADDI, "DR %d SR1 %d imm5 %d", a->DR, a->SR1, a->imm5)
INSN(LD,   "DR %d PCoffset9 %d", a->DR, a->PCoffset9)
INSN(ST,   "SR %d PCoffset9 %d", a->SR, a->PCoffset9)
INSN(JSR,  "PCoffset11 %d", a->PCoffset11)
INSN(JSRR, "BaseR %d", a->BaseR)
INSN(AND,  "DR %d SR1 %d SR2 %d", a->DR, a->SR1, a->SR2)
INSN(ANDI, "DR %d SR1 %d imm5 %d", a->DR, a->SR1, a->imm5)
INSN(LDR,  "DR %d BaseR %d offset6 %d", a->DR, a->BaseR, a->offset6)
INSN(STR,  "SR %d BaseR %d offset6 %d", a->SR, a->BaseR, a->offset6)
INSN(RTI,  "")
INSN(NOT,  "DR %d SR %d", a->DR, a->SR)
INSN(LDI,  "DR %d PCoffset9 %d", a->DR, a->PCoffset9)
INSN(STI,  "SR %d PCoffset9 %d", a->SR, a->PCoffset9)
INSN(JMP,  "BaseR %d", a->BaseR)
INSN(LEA,  "DR %d PCoffset9 %d", a->DR, a->PCoffset9)
INSN(TRAP, "trapvect8 %d", a->trapvect8)