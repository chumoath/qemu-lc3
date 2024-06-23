/*
 * QEMU AVR CPU
 *
 * Copyright (c) 2019-2020 Michael Rolnik
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, see
 * <http://www.gnu.org/licenses/lgpl-2.1.html>
 */

#include "qemu/osdep.h"
#include "qemu/qemu-print.h"
#include "tcg/tcg.h"
#include "cpu.h"
#include "exec/exec-all.h"
#include "tcg/tcg-op.h"
#include "exec/cpu_ldst.h"
#include "exec/helper-proto.h"
#include "exec/helper-gen.h"
#include "exec/log.h"
#include "exec/translator.h"

#define HELPER_H "helper.h"
#include "exec/helper-info.c.inc"
#undef  HELPER_H


/*
 *  Define if you want a BREAK instruction translated to a breakpoint
 *  Active debugging connection is assumed
 *  This is for
 *  https://github.com/seharris/qemu-avr-tests/tree/master/instruction-tests
 *  tests
 */
#undef BREAKPOINT_ON_BREAK

static TCGv cpu_pc;

static TCGv cpu_Cf;
static TCGv cpu_Zf;
static TCGv cpu_Nf;
static TCGv cpu_Vf;
static TCGv cpu_Sf;
static TCGv cpu_Hf;
static TCGv cpu_Tf;
static TCGv cpu_If;

static TCGv cpu_rampD;
static TCGv cpu_rampX;
static TCGv cpu_rampY;
static TCGv cpu_rampZ;

static TCGv cpu_r[NUMBER_OF_CPU_REGISTERS];
static TCGv cpu_eind;
static TCGv cpu_sp;

static TCGv cpu_skip;

static const char reg_names[NUMBER_OF_CPU_REGISTERS][8] = {
    "r0",  "r1",  "r2",  "r3",  "r4",  "r5",  "r6",  "r7",
    "r8",  "r9",  "r10", "r11", "r12", "r13", "r14", "r15",
    "r16", "r17", "r18", "r19", "r20", "r21", "r22", "r23",
    "r24", "r25", "r26", "r27", "r28", "r29", "r30", "r31",
};
#define REG(x) (cpu_r[x])

#define DISAS_EXIT   DISAS_TARGET_0  /* We want return to the cpu main loop.  */
#define DISAS_LOOKUP DISAS_TARGET_1  /* We have a variable condition exit.  */
#define DISAS_CHAIN  DISAS_TARGET_2  /* We have a single condition exit.  */

typedef struct DisasContext DisasContext;

/* This is the state at translation time. */
struct DisasContext {
    DisasContextBase base;

    CPUAVRState *env;
    CPUState *cs;

    target_long npc;
    uint32_t opcode;

    /* Routine used to access memory */
    int memidx;

    /*
     * some AVR instructions can make the following instruction to be skipped
     * Let's name those instructions
     *     A   - instruction that can skip the next one
     *     B   - instruction that can be skipped. this depends on execution of A
     * there are two scenarios
     * 1. A and B belong to the same translation block
     * 2. A is the last instruction in the translation block and B is the last
     *
     * following variables are used to simplify the skipping logic, they are
     * used in the following manner (sketch)
     *
     * TCGLabel *skip_label = NULL;
     * if (ctx->skip_cond != TCG_COND_NEVER) {
     *     skip_label = gen_new_label();
     *     tcg_gen_brcond_tl(skip_cond, skip_var0, skip_var1, skip_label);
     * }
     *
     * translate(ctx);
     *
     * if (skip_label) {
     *     gen_set_label(skip_label);
     * }
     */
    TCGv skip_var0;
    TCGv skip_var1;
    TCGCond skip_cond;
};

void avr_cpu_tcg_init(void)
{
    int i;

#define AVR_REG_OFFS(x) offsetof(CPUAVRState, x)
    cpu_pc = tcg_global_mem_new_i32(cpu_env, AVR_REG_OFFS(pc_w), "pc");
    cpu_Cf = tcg_global_mem_new_i32(cpu_env, AVR_REG_OFFS(sregC), "Cf");
    cpu_Zf = tcg_global_mem_new_i32(cpu_env, AVR_REG_OFFS(sregZ), "Zf");
    cpu_Nf = tcg_global_mem_new_i32(cpu_env, AVR_REG_OFFS(sregN), "Nf");
    cpu_Vf = tcg_global_mem_new_i32(cpu_env, AVR_REG_OFFS(sregV), "Vf");
    cpu_Sf = tcg_global_mem_new_i32(cpu_env, AVR_REG_OFFS(sregS), "Sf");
    cpu_Hf = tcg_global_mem_new_i32(cpu_env, AVR_REG_OFFS(sregH), "Hf");
    cpu_Tf = tcg_global_mem_new_i32(cpu_env, AVR_REG_OFFS(sregT), "Tf");
    cpu_If = tcg_global_mem_new_i32(cpu_env, AVR_REG_OFFS(sregI), "If");
    cpu_rampD = tcg_global_mem_new_i32(cpu_env, AVR_REG_OFFS(rampD), "rampD");
    cpu_rampX = tcg_global_mem_new_i32(cpu_env, AVR_REG_OFFS(rampX), "rampX");
    cpu_rampY = tcg_global_mem_new_i32(cpu_env, AVR_REG_OFFS(rampY), "rampY");
    cpu_rampZ = tcg_global_mem_new_i32(cpu_env, AVR_REG_OFFS(rampZ), "rampZ");
    cpu_eind = tcg_global_mem_new_i32(cpu_env, AVR_REG_OFFS(eind), "eind");
    cpu_sp = tcg_global_mem_new_i32(cpu_env, AVR_REG_OFFS(sp), "sp");
    cpu_skip = tcg_global_mem_new_i32(cpu_env, AVR_REG_OFFS(skip), "skip");

    for (i = 0; i < NUMBER_OF_CPU_REGISTERS; i++) {
        cpu_r[i] = tcg_global_mem_new_i32(cpu_env, AVR_REG_OFFS(r[i]),
                                          reg_names[i]);
    }
#undef AVR_REG_OFFS
}

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
    return cpu_lduw_code(ctx->env, ctx->npc++ * 2);
}

static int append_16(DisasContext *ctx, int x)
{
    return x << 16 | next_word(ctx);
}

static bool avr_have_feature(DisasContext *ctx, int feature)
{
    if (!avr_feature(ctx->env, feature)) {
        gen_helper_unsupported(cpu_env);
        ctx->base.is_jmp = DISAS_NORETURN;
        return false;
    }
    return true;
}

static bool decode_insn(DisasContext *ctx, uint16_t insn);
#include "decode-insn.c.inc"

/*
 * Arithmetic Instructions
 */

/*
 * Utility functions for updating status registers:
 *
 *   - gen_add_CHf()
 *   - gen_add_Vf()
 *   - gen_sub_CHf()
 *   - gen_sub_Vf()
 *   - gen_NSf()
 *   - gen_ZNSf()
 *
 */

static void gen_add_CHf(TCGv R, TCGv Rd, TCGv Rr)
{
    TCGv t1 = tcg_temp_new_i32();
    TCGv t2 = tcg_temp_new_i32();
    TCGv t3 = tcg_temp_new_i32();

    tcg_gen_and_tl(t1, Rd, Rr); /* t1 = Rd & Rr */
    tcg_gen_andc_tl(t2, Rd, R); /* t2 = Rd & ~R */
    tcg_gen_andc_tl(t3, Rr, R); /* t3 = Rr & ~R */
    tcg_gen_or_tl(t1, t1, t2); /* t1 = t1 | t2 | t3 */
    tcg_gen_or_tl(t1, t1, t3);

    tcg_gen_shri_tl(cpu_Cf, t1, 7); /* Cf = t1(7) */
    tcg_gen_shri_tl(cpu_Hf, t1, 3); /* Hf = t1(3) */
    tcg_gen_andi_tl(cpu_Hf, cpu_Hf, 1);
}

static void gen_add_Vf(TCGv R, TCGv Rd, TCGv Rr)
{
    TCGv t1 = tcg_temp_new_i32();
    TCGv t2 = tcg_temp_new_i32();

    /* t1 = Rd & Rr & ~R | ~Rd & ~Rr & R */
    /*    = (Rd ^ R) & ~(Rd ^ Rr) */
    tcg_gen_xor_tl(t1, Rd, R);
    tcg_gen_xor_tl(t2, Rd, Rr);
    tcg_gen_andc_tl(t1, t1, t2);

    tcg_gen_shri_tl(cpu_Vf, t1, 7); /* Vf = t1(7) */
}

static void gen_sub_CHf(TCGv R, TCGv Rd, TCGv Rr)
{
    TCGv t1 = tcg_temp_new_i32();
    TCGv t2 = tcg_temp_new_i32();
    TCGv t3 = tcg_temp_new_i32();

    tcg_gen_not_tl(t1, Rd); /* t1 = ~Rd */
    tcg_gen_and_tl(t2, t1, Rr); /* t2 = ~Rd & Rr */
    tcg_gen_or_tl(t3, t1, Rr); /* t3 = (~Rd | Rr) & R */
    tcg_gen_and_tl(t3, t3, R);
    tcg_gen_or_tl(t2, t2, t3); /* t2 = ~Rd & Rr | ~Rd & R | R & Rr */

    tcg_gen_shri_tl(cpu_Cf, t2, 7); /* Cf = t2(7) */
    tcg_gen_shri_tl(cpu_Hf, t2, 3); /* Hf = t2(3) */
    tcg_gen_andi_tl(cpu_Hf, cpu_Hf, 1);
}

static void gen_sub_Vf(TCGv R, TCGv Rd, TCGv Rr)
{
    TCGv t1 = tcg_temp_new_i32();
    TCGv t2 = tcg_temp_new_i32();

    /* t1 = Rd & ~Rr & ~R | ~Rd & Rr & R */
    /*    = (Rd ^ R) & (Rd ^ R) */
    tcg_gen_xor_tl(t1, Rd, R);
    tcg_gen_xor_tl(t2, Rd, Rr);
    tcg_gen_and_tl(t1, t1, t2);

    tcg_gen_shri_tl(cpu_Vf, t1, 7); /* Vf = t1(7) */
}

static void gen_NSf(TCGv R)
{
    tcg_gen_shri_tl(cpu_Nf, R, 7); /* Nf = R(7) */
    tcg_gen_xor_tl(cpu_Sf, cpu_Nf, cpu_Vf); /* Sf = Nf ^ Vf */
}

static void gen_ZNSf(TCGv R)
{
    tcg_gen_setcondi_tl(TCG_COND_EQ, cpu_Zf, R, 0); /* Zf = R == 0 */

    /* update status register */
    tcg_gen_shri_tl(cpu_Nf, R, 7); /* Nf = R(7) */
    tcg_gen_xor_tl(cpu_Sf, cpu_Nf, cpu_Vf); /* Sf = Nf ^ Vf */
}

/*
 * Branch Instructions
 */
static void gen_jmp_ez(DisasContext *ctx)
{
    tcg_gen_deposit_tl(cpu_pc, cpu_r[30], cpu_r[31], 8, 8);
    tcg_gen_or_tl(cpu_pc, cpu_pc, cpu_eind);
    ctx->base.is_jmp = DISAS_LOOKUP;
}

static void gen_jmp_z(DisasContext *ctx)
{
    tcg_gen_deposit_tl(cpu_pc, cpu_r[30], cpu_r[31], 8, 8);
    ctx->base.is_jmp = DISAS_LOOKUP;
}

static void gen_push_ret(DisasContext *ctx, int ret)
{
    if (avr_feature(ctx->env, AVR_FEATURE_1_BYTE_PC)) {
        TCGv t0 = tcg_constant_i32(ret & 0x0000ff);

        tcg_gen_qemu_st_tl(t0, cpu_sp, MMU_DATA_IDX, MO_UB);
        tcg_gen_subi_tl(cpu_sp, cpu_sp, 1);
    } else if (avr_feature(ctx->env, AVR_FEATURE_2_BYTE_PC)) {
        TCGv t0 = tcg_constant_i32(ret & 0x00ffff);

        tcg_gen_subi_tl(cpu_sp, cpu_sp, 1);
        tcg_gen_qemu_st_tl(t0, cpu_sp, MMU_DATA_IDX, MO_BEUW);
        tcg_gen_subi_tl(cpu_sp, cpu_sp, 1);
    } else if (avr_feature(ctx->env, AVR_FEATURE_3_BYTE_PC)) {
        TCGv lo = tcg_constant_i32(ret & 0x0000ff);
        TCGv hi = tcg_constant_i32((ret & 0xffff00) >> 8);

        tcg_gen_qemu_st_tl(lo, cpu_sp, MMU_DATA_IDX, MO_UB);
        tcg_gen_subi_tl(cpu_sp, cpu_sp, 2);
        tcg_gen_qemu_st_tl(hi, cpu_sp, MMU_DATA_IDX, MO_BEUW);
        tcg_gen_subi_tl(cpu_sp, cpu_sp, 1);
    }
}

static void gen_pop_ret(DisasContext *ctx, TCGv ret)
{
    if (avr_feature(ctx->env, AVR_FEATURE_1_BYTE_PC)) {
        tcg_gen_addi_tl(cpu_sp, cpu_sp, 1);
        tcg_gen_qemu_ld_tl(ret, cpu_sp, MMU_DATA_IDX, MO_UB);
    } else if (avr_feature(ctx->env, AVR_FEATURE_2_BYTE_PC)) {
        tcg_gen_addi_tl(cpu_sp, cpu_sp, 1);
        tcg_gen_qemu_ld_tl(ret, cpu_sp, MMU_DATA_IDX, MO_BEUW);
        tcg_gen_addi_tl(cpu_sp, cpu_sp, 1);
    } else if (avr_feature(ctx->env, AVR_FEATURE_3_BYTE_PC)) {
        TCGv lo = tcg_temp_new_i32();
        TCGv hi = tcg_temp_new_i32();

        tcg_gen_addi_tl(cpu_sp, cpu_sp, 1);
        tcg_gen_qemu_ld_tl(hi, cpu_sp, MMU_DATA_IDX, MO_BEUW);

        tcg_gen_addi_tl(cpu_sp, cpu_sp, 2);
        tcg_gen_qemu_ld_tl(lo, cpu_sp, MMU_DATA_IDX, MO_UB);

        tcg_gen_deposit_tl(ret, lo, hi, 8, 16);
    }
}

static void gen_goto_tb(DisasContext *ctx, int n, target_ulong dest)
{
    const TranslationBlock *tb = ctx->base.tb;

    if (translator_use_goto_tb(&ctx->base, dest)) {
        tcg_gen_goto_tb(n);
        tcg_gen_movi_i32(cpu_pc, dest);
        tcg_gen_exit_tb(tb, n);
    } else {
        tcg_gen_movi_i32(cpu_pc, dest);
        tcg_gen_lookup_and_goto_ptr();
    }
    ctx->base.is_jmp = DISAS_NORETURN;
}

/*
 * Data Transfer Instructions
 */

/*
 *  in the gen_set_addr & gen_get_addr functions
 *  H assumed to be in 0x00ff0000 format
 *  M assumed to be in 0x000000ff format
 *  L assumed to be in 0x000000ff format
 */
static void gen_set_addr(TCGv addr, TCGv H, TCGv M, TCGv L)
{

    tcg_gen_andi_tl(L, addr, 0x000000ff);

    tcg_gen_andi_tl(M, addr, 0x0000ff00);
    tcg_gen_shri_tl(M, M, 8);

    tcg_gen_andi_tl(H, addr, 0x00ff0000);
}

static void gen_set_xaddr(TCGv addr)
{
    gen_set_addr(addr, cpu_rampX, cpu_r[27], cpu_r[26]);
}

static void gen_set_yaddr(TCGv addr)
{
    gen_set_addr(addr, cpu_rampY, cpu_r[29], cpu_r[28]);
}

static void gen_set_zaddr(TCGv addr)
{
    gen_set_addr(addr, cpu_rampZ, cpu_r[31], cpu_r[30]);
}

static TCGv gen_get_addr(TCGv H, TCGv M, TCGv L)
{
    TCGv addr = tcg_temp_new_i32();

    tcg_gen_deposit_tl(addr, M, H, 8, 8);
    tcg_gen_deposit_tl(addr, L, addr, 8, 16);

    return addr;
}

static TCGv gen_get_xaddr(void)
{
    return gen_get_addr(cpu_rampX, cpu_r[27], cpu_r[26]);
}

static TCGv gen_get_yaddr(void)
{
    return gen_get_addr(cpu_rampY, cpu_r[29], cpu_r[28]);
}

static TCGv gen_get_zaddr(void)
{
    return gen_get_addr(cpu_rampZ, cpu_r[31], cpu_r[30]);
}

/*
 *  Load one byte indirect from data space to register and stores an clear
 *  the bits in data space specified by the register. The instruction can only
 *  be used towards internal SRAM.  The data location is pointed to by the Z (16
 *  bits) Pointer Register in the Register File. Memory access is limited to the
 *  current data segment of 64KB. To access another data segment in devices with
 *  more than 64KB data space, the RAMPZ in register in the I/O area has to be
 *  changed.  The Z-pointer Register is left unchanged by the operation. This
 *  instruction is especially suited for clearing status bits stored in SRAM.
 */
static void gen_data_store(DisasContext *ctx, TCGv data, TCGv addr)
{
    if (ctx->base.tb->flags & TB_FLAGS_FULL_ACCESS) {
        gen_helper_fullwr(cpu_env, data, addr);
    } else {
        tcg_gen_qemu_st_tl(data, addr, MMU_DATA_IDX, MO_UB);
    }
}

static void gen_data_load(DisasContext *ctx, TCGv data, TCGv addr)
{
    if (ctx->base.tb->flags & TB_FLAGS_FULL_ACCESS) {
        gen_helper_fullrd(data, cpu_env, addr);
    } else {
        tcg_gen_qemu_ld_tl(data, addr, MMU_DATA_IDX, MO_UB);
    }
}

static bool trans_ADD (DisasContext *ctx, arg_ADD *a)
{
    return true;
}
static bool trans_ADDI(DisasContext *ctx, arg_ADDI *a)
{
    return true;
}
static bool trans_AND (DisasContext *ctx, arg_AND *a)
{
    return true;
}
static bool trans_ANDI(DisasContext *ctx, arg_ANDI *a)
{
    return true;
}
static bool trans_BR  (DisasContext *ctx, arg_BR *a)
{
    return true;
}
static bool trans_JMP (DisasContext *ctx, arg_JMP *a)
{
    return true;
}
static bool trans_JSR (DisasContext *ctx, arg_JSR *a)
{
    return true;
}
static bool trans_JSRR(DisasContext *ctx, arg_JSRR *a)
{
    return true;
}
static bool trans_LD  (DisasContext *ctx, arg_LD *a)
{
    return true;
}
static bool trans_LDI (DisasContext *ctx, arg_LDI *a)
{
    return true;
}
static bool trans_LDR (DisasContext *ctx, arg_LDR *a)
{
    return true;
}
static bool trans_LEA (DisasContext *ctx, arg_LEA *a)
{
    return true;
}
static bool trans_NOT (DisasContext *ctx, arg_NOT *a)
{
    return true;
}
static bool trans_RTI (DisasContext *ctx, arg_RTI *a)
{
    return true;
}
static bool trans_ST  (DisasContext *ctx, arg_ST *a)
{
    return true;
}
static bool trans_STI (DisasContext *ctx, arg_STI *a)
{
    return true;
}
static bool trans_STR (DisasContext *ctx, arg_STR *a)
{
    return true;
}
static bool trans_TRAP(DisasContext *ctx, arg_TRAP *a)
{
    return true;
}

/*
 *  Core translation mechanism functions:
 *
 *    - translate()
 *    - canonicalize_skip()
 *    - gen_intermediate_code()
 *    - restore_state_to_opc()
 *
 */
static void translate(DisasContext *ctx)
{
    uint32_t opcode = next_word(ctx);

    if (!decode_insn(ctx, opcode)) {
        gen_helper_unsupported(cpu_env);
        ctx->base.is_jmp = DISAS_NORETURN;
    }
}

/* Standardize the cpu_skip condition to NE.  */
static bool canonicalize_skip(DisasContext *ctx)
{
    switch (ctx->skip_cond) {
    case TCG_COND_NEVER:
        /* Normal case: cpu_skip is known to be false.  */
        return false;

    case TCG_COND_ALWAYS:
        /*
         * Breakpoint case: cpu_skip is known to be true, via TB_FLAGS_SKIP.
         * The breakpoint is on the instruction being skipped, at the start
         * of the TranslationBlock.  No need to update.
         */
        return false;

    case TCG_COND_NE:
        if (ctx->skip_var1 == NULL) {
            tcg_gen_mov_tl(cpu_skip, ctx->skip_var0);
        } else {
            tcg_gen_xor_tl(cpu_skip, ctx->skip_var0, ctx->skip_var1);
            ctx->skip_var1 = NULL;
        }
        break;

    default:
        /* Convert to a NE condition vs 0. */
        if (ctx->skip_var1 == NULL) {
            tcg_gen_setcondi_tl(ctx->skip_cond, cpu_skip, ctx->skip_var0, 0);
        } else {
            tcg_gen_setcond_tl(ctx->skip_cond, cpu_skip,
                               ctx->skip_var0, ctx->skip_var1);
            ctx->skip_var1 = NULL;
        }
        ctx->skip_cond = TCG_COND_NE;
        break;
    }
    ctx->skip_var0 = cpu_skip;
    return true;
}

static void avr_tr_init_disas_context(DisasContextBase *dcbase, CPUState *cs)
{
    DisasContext *ctx = container_of(dcbase, DisasContext, base);
    CPUAVRState *env = cs->env_ptr;
    uint32_t tb_flags = ctx->base.tb->flags;

    ctx->cs = cs;
    ctx->env = env;
    ctx->npc = ctx->base.pc_first / 2;

    ctx->skip_cond = TCG_COND_NEVER;
    if (tb_flags & TB_FLAGS_SKIP) {
        ctx->skip_cond = TCG_COND_ALWAYS;
        ctx->skip_var0 = cpu_skip;
    }

    if (tb_flags & TB_FLAGS_FULL_ACCESS) {
        /*
         * This flag is set by ST/LD instruction we will regenerate it ONLY
         * with mem/cpu memory access instead of mem access
         */
        ctx->base.max_insns = 1;
    }
}

static void avr_tr_tb_start(DisasContextBase *db, CPUState *cs)
{
}

static void avr_tr_insn_start(DisasContextBase *dcbase, CPUState *cs)
{
    DisasContext *ctx = container_of(dcbase, DisasContext, base);

    tcg_gen_insn_start(ctx->npc);
}

static void avr_tr_translate_insn(DisasContextBase *dcbase, CPUState *cs)
{
    DisasContext *ctx = container_of(dcbase, DisasContext, base);
    TCGLabel *skip_label = NULL;

    /* Conditionally skip the next instruction, if indicated.  */
    if (ctx->skip_cond != TCG_COND_NEVER) {
        skip_label = gen_new_label();
        if (ctx->skip_var0 == cpu_skip) {
            /*
             * Copy cpu_skip so that we may zero it before the branch.
             * This ensures that cpu_skip is non-zero after the label
             * if and only if the skipped insn itself sets a skip.
             */
            ctx->skip_var0 = tcg_temp_new();
            tcg_gen_mov_tl(ctx->skip_var0, cpu_skip);
            tcg_gen_movi_tl(cpu_skip, 0);
        }
        if (ctx->skip_var1 == NULL) {
            tcg_gen_brcondi_tl(ctx->skip_cond, ctx->skip_var0, 0, skip_label);
        } else {
            tcg_gen_brcond_tl(ctx->skip_cond, ctx->skip_var0,
                              ctx->skip_var1, skip_label);
            ctx->skip_var1 = NULL;
        }
        ctx->skip_cond = TCG_COND_NEVER;
        ctx->skip_var0 = NULL;
    }

    translate(ctx);

    ctx->base.pc_next = ctx->npc * 2;

    if (skip_label) {
        canonicalize_skip(ctx);
        gen_set_label(skip_label);

        switch (ctx->base.is_jmp) {
        case DISAS_NORETURN:
            ctx->base.is_jmp = DISAS_CHAIN;
            break;
        case DISAS_NEXT:
            if (ctx->base.tb->flags & TB_FLAGS_SKIP) {
                ctx->base.is_jmp = DISAS_TOO_MANY;
            }
            break;
        default:
            break;
        }
    }

    if (ctx->base.is_jmp == DISAS_NEXT) {
        target_ulong page_first = ctx->base.pc_first & TARGET_PAGE_MASK;

        if ((ctx->base.pc_next - page_first) >= TARGET_PAGE_SIZE - 4) {
            ctx->base.is_jmp = DISAS_TOO_MANY;
        }
    }
}

static void avr_tr_tb_stop(DisasContextBase *dcbase, CPUState *cs)
{
    DisasContext *ctx = container_of(dcbase, DisasContext, base);
    bool nonconst_skip = canonicalize_skip(ctx);
    /*
     * Because we disable interrupts while env->skip is set,
     * we must return to the main loop to re-evaluate afterward.
     */
    bool force_exit = ctx->base.tb->flags & TB_FLAGS_SKIP;

    switch (ctx->base.is_jmp) {
    case DISAS_NORETURN:
        assert(!nonconst_skip);
        break;
    case DISAS_NEXT:
    case DISAS_TOO_MANY:
    case DISAS_CHAIN:
        if (!nonconst_skip && !force_exit) {
            /* Note gen_goto_tb checks singlestep.  */
            gen_goto_tb(ctx, 1, ctx->npc);
            break;
        }
        tcg_gen_movi_tl(cpu_pc, ctx->npc);
        /* fall through */
    case DISAS_LOOKUP:
        if (!force_exit) {
            tcg_gen_lookup_and_goto_ptr();
            break;
        }
        /* fall through */
    case DISAS_EXIT:
        tcg_gen_exit_tb(NULL, 0);
        break;
    default:
        g_assert_not_reached();
    }
}

static void avr_tr_disas_log(const DisasContextBase *dcbase,
                             CPUState *cs, FILE *logfile)
{
    fprintf(logfile, "IN: %s\n", lookup_symbol(dcbase->pc_first));
    target_disas(logfile, cs, dcbase->pc_first, dcbase->tb->size);
}

static const TranslatorOps avr_tr_ops = {
    .init_disas_context = avr_tr_init_disas_context,
    .tb_start           = avr_tr_tb_start,
    .insn_start         = avr_tr_insn_start,
    .translate_insn     = avr_tr_translate_insn,
    .tb_stop            = avr_tr_tb_stop,
    .disas_log          = avr_tr_disas_log,
};

void gen_intermediate_code(CPUState *cs, TranslationBlock *tb, int *max_insns,
                           target_ulong pc, void *host_pc)
{
    DisasContext dc = { };
    translator_loop(cs, tb, max_insns, pc, host_pc, &avr_tr_ops, &dc.base);
}
