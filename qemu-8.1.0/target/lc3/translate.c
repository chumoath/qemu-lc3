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
#include "qemu/datadir.h"
#include "hw/loader.h"

#define HELPER_H "helper.h"
#include "exec/helper-info.c.inc"
#undef  HELPER_H


#undef BREAKPOINT_ON_BREAK

static TCGv cpu_pc;

static TCGv cpu_p;
static TCGv cpu_z;
static TCGv cpu_n;
static TCGv cpu_r[NUMBER_OF_CPU_REGISTERS];

static const char reg_names[NUMBER_OF_CPU_REGISTERS][8] = {
    "r0",  "r1",  "r2",  "r3",  "r4",  "r5",  "r6",  "r7",
};
#define REG(x) (cpu_r[x])

#define DISAS_EXIT   DISAS_TARGET_0  /* We want return to the cpu main loop.  */
#define DISAS_LOOKUP DISAS_TARGET_1  /* We have a variable condition exit.  */
#define DISAS_CHAIN  DISAS_TARGET_2  /* We have a single condition exit.  */

typedef struct DisasContext DisasContext;

/* This is the state at translation time. */
struct DisasContext {
    DisasContextBase base;

    CPULC3State *env;
    CPUState *cs;

    target_long npc;
    uint32_t opcode;
    int memidx;
};

void lc3_cpu_tcg_init(void)
{
    int i;

#define LC3_REG_OFFS(x) offsetof(CPULC3State, x)
    cpu_pc = tcg_global_mem_new_i32(cpu_env, LC3_REG_OFFS(R_PC), "pc");
    cpu_p = tcg_global_mem_new_i32(cpu_env, LC3_REG_OFFS(R_P), "p");
    cpu_z = tcg_global_mem_new_i32(cpu_env, LC3_REG_OFFS(R_Z), "z");
    cpu_n = tcg_global_mem_new_i32(cpu_env, LC3_REG_OFFS(R_N), "n");

    for (i = 0; i < NUMBER_OF_CPU_REGISTERS; i++) {
        cpu_r[i] = tcg_global_mem_new_i32(cpu_env, LC3_REG_OFFS(r[i]), reg_names[i]);
    }
#undef LC3_REG_OFFS
}

static int sign_extend(int x, int bit_count)
{
    if ((x >> (bit_count - 1)) & 1) {
        x |= (0xFFFFFFFF << bit_count);
    }
    return x;
}

static uint16_t next_word(DisasContext *ctx)
{
    uint16_t ret;
    address_space_read(&address_space_memory, ctx->npc++ * 2, MEMTXATTRS_UNSPECIFIED, &ret, 2);
    return ret;
    // return cpu_lduw_code(ctx->env, ctx->npc++ * 2);
}

static bool decode_insn(DisasContext *ctx, uint16_t insn);
#include "decode-insn.c.inc"

static void gen_update_flags(TCGv dr)
{
    tcg_gen_setcondi_tl(TCG_COND_EQ, cpu_z, dr, 0);

    tcg_gen_shri_tl(cpu_n, dr, 15);
    tcg_gen_andi_tl(cpu_n, cpu_n, 1);

    // N Z
    // 0 0
    // 1 0
    // 0 1
    tcg_gen_xor_tl(cpu_p, cpu_z, cpu_n);
    tcg_gen_setcondi_tl(TCG_COND_EQ, cpu_p, cpu_p, 0);
}

static void gen_goto_tb(DisasContext *ctx, int n, target_ulong dest)
{
    const TranslationBlock *tb = ctx->base.tb;

    // pc_first ^ dest * 2；如果不 x2，永远不会 block chain
    if (translator_use_goto_tb(&ctx->base, dest * 2)) {
        tcg_gen_goto_tb(n);
        tcg_gen_movi_i32(cpu_pc, dest);
        tcg_gen_exit_tb(tb, n);
    } else {
        tcg_gen_movi_i32(cpu_pc, dest);
        tcg_gen_lookup_and_goto_ptr();
    }
    ctx->base.is_jmp = DISAS_NORETURN;
}

static void gen_data_store(DisasContext *ctx, TCGv data, TCGv addr)
{
    gen_helper_fullwr(cpu_env, data, addr);
}

static void gen_data_load(DisasContext *ctx, TCGv data, TCGv addr)
{
    gen_helper_fullrd(data, cpu_env, addr);
}

static bool trans_ADD (DisasContext *ctx, arg_ADD *a)
{
    TCGv dr = cpu_r[a->DR];
    TCGv sr1 = cpu_r[a->SR1];
    TCGv sr2 = cpu_r[a->SR2];
    TCGv npc = tcg_constant_tl(ctx->npc);
    TCGv_ptr fmt = tcg_constant_ptr("ADD: \n");

    tcg_gen_add_tl(dr, sr1, sr2);
    tcg_gen_andi_tl(dr, dr, 0xffff);
    gen_update_flags(dr);
    gen_helper_print_regs(cpu_env, fmt, npc);
    return true;
}
static bool trans_ADDI(DisasContext *ctx, arg_ADDI *a)
{
    TCGv dr = cpu_r[a->DR];
    TCGv sr1 = cpu_r[a->SR1];
    TCGv npc = tcg_constant_tl(ctx->npc);
    TCGv_ptr fmt = tcg_constant_ptr("ADDI: \n");

    tcg_gen_addi_tl(dr, sr1, sign_extend(a->imm5, 5));
    tcg_gen_andi_tl(dr, dr, 0xffff);
    gen_update_flags(dr);
    gen_helper_print_regs(cpu_env, fmt, npc);

    return true;
}
static bool trans_AND (DisasContext *ctx, arg_AND *a)
{
    TCGv dr = cpu_r[a->DR];
    TCGv sr1 = cpu_r[a->SR1];
    TCGv sr2 = cpu_r[a->SR2];
    TCGv npc = tcg_constant_tl(ctx->npc);
    TCGv_ptr fmt = tcg_constant_ptr("AND: \n");
    
    tcg_gen_and_tl(dr, sr1, sr2);
    tcg_gen_andi_tl(dr, dr, 0xffff);
    gen_update_flags(dr);
    gen_helper_print_regs(cpu_env, fmt, npc);

    return true;
}
static bool trans_ANDI(DisasContext *ctx, arg_ANDI *a)
{
    TCGv dr = cpu_r[a->DR];
    TCGv sr1 = cpu_r[a->SR1];
    TCGv npc = tcg_constant_tl(ctx->npc);
    TCGv_ptr fmt = tcg_constant_ptr("ANDI: \n");
    
    tcg_gen_andi_tl(dr, sr1, sign_extend(a->imm5, 5));
    tcg_gen_andi_tl(dr, dr, 0xffff);
    gen_update_flags(dr);
    gen_helper_print_regs(cpu_env, fmt, npc);

    return true;
}
static bool trans_BR  (DisasContext *ctx, arg_BR *a)
{
    TCGv is_jmp = tcg_temp_new_i32();
    TCGv is_jmp2 = tcg_temp_new_i32();
    TCGLabel *not_taken = gen_new_label();
    TCGv npc = tcg_constant_tl(ctx->npc);
    TCGv_ptr fmt = tcg_constant_ptr("BR: \n");
    TCGv_ptr fmt0 = tcg_constant_ptr("BR: 0\n");
    TCGv_ptr fmt1 = tcg_constant_ptr("BR: 1\n");
    
    tcg_gen_andi_tl(is_jmp, cpu_z, a->z);
    tcg_gen_andi_tl(is_jmp, is_jmp, 1);

    tcg_gen_andi_tl(is_jmp2, cpu_n, a->n);
    tcg_gen_andi_tl(is_jmp2, is_jmp2, 1);

    tcg_gen_or_tl(is_jmp, is_jmp, is_jmp2);

    tcg_gen_andi_tl(is_jmp2, cpu_p, a->p);
    tcg_gen_andi_tl(is_jmp2, is_jmp2, 1);

    tcg_gen_or_tl(is_jmp, is_jmp, is_jmp2);

    gen_helper_print_regs(cpu_env, fmt, npc);

    tcg_gen_brcondi_i32(TCG_COND_EQ, is_jmp, 0, not_taken);
    gen_helper_print_regs(cpu_env, fmt1, npc);
    gen_goto_tb(ctx, 0, ctx->npc + sign_extend(a->PCoffset9, 9));

    gen_set_label(not_taken);
    gen_helper_print_regs(cpu_env, fmt0, npc);

    ctx->base.is_jmp = DISAS_CHAIN;
    return true;
}

static bool trans_JMP (DisasContext *ctx, arg_JMP *a)
{
    TCGv npc = tcg_constant_tl(ctx->npc);
    TCGv_ptr fmt = tcg_constant_ptr("JMP: \n");

    gen_helper_print_regs(cpu_env, fmt, npc);
    // 必须使用运行时的变量，不能使用编译时的定值；只能使用 pc 和 TCGv
    // gen_goto_tb(ctx, 0, ctx->env->r[a->BaseR]);
    // gen_goto_tb(ctx, 0, cpu_r[a->BaseR]);
    tcg_gen_mov_tl(cpu_pc, cpu_r[a->BaseR]);
    ctx->base.is_jmp = DISAS_LOOKUP;

    return true;
}

static bool trans_JSR (DisasContext *ctx, arg_JSR *a)
{
    TCGv npc = tcg_constant_tl(ctx->npc);
    TCGv_ptr fmt = tcg_constant_ptr("JSR: \n");
    
    tcg_gen_movi_tl(cpu_r[7], ctx->npc);

    gen_helper_print_regs(cpu_env, fmt, npc);
    gen_goto_tb(ctx, 0, (ctx->npc + sign_extend(a->PCoffset11, 11)) & 0xffff);

    return true;
}

static bool trans_JSRR(DisasContext *ctx, arg_JSRR *a)
{
    TCGv npc = tcg_constant_tl(ctx->npc);
    TCGv_ptr fmt = tcg_constant_ptr("JSRR: \n");
    
    tcg_gen_movi_tl(cpu_r[7], ctx->npc);

    gen_helper_print_regs(cpu_env, fmt, npc);
    // 必须使用运行时的变量，不能使用编译时的定值；只能使用 pc 和 TCGv
    // gen_goto_tb(ctx, 0, ctx->env->r[a->BaseR]);
    // gen_goto_tb(ctx, 0, cpu_r[a->BaseR]);
    tcg_gen_mov_tl(cpu_pc, cpu_r[a->BaseR]);
    ctx->base.is_jmp = DISAS_LOOKUP;
    return true;
}

static bool trans_LD  (DisasContext *ctx, arg_LD *a)
{
    TCGv dr = cpu_r[a->DR];
    TCGv addr = tcg_temp_new_i32();
    TCGv npc = tcg_constant_tl(ctx->npc);
    TCGv_ptr fmt = tcg_constant_ptr("LD: \n");
    
    tcg_gen_movi_tl(addr, ctx->npc);
    tcg_gen_addi_tl(addr, addr, sign_extend(a->PCoffset9, 9));
    gen_data_load(ctx, dr, addr);
    tcg_gen_andi_tl(dr, dr, 0xffff);
    gen_update_flags(dr);

    gen_helper_print_regs(cpu_env, fmt, npc);

    return true;
}

static bool trans_LDI (DisasContext *ctx, arg_LDI *a)
{
    TCGv dr = cpu_r[a->DR];
    TCGv addr = tcg_temp_new_i32();
    TCGv addr1 = tcg_temp_new_i32();
    TCGv npc = tcg_constant_tl(ctx->npc);
    TCGv_ptr fmt = tcg_constant_ptr("LDI: \n");
    
    tcg_gen_movi_tl(addr, ctx->npc);
    tcg_gen_addi_tl(addr, addr, sign_extend(a->PCoffset9, 9));
    gen_data_load(ctx, addr1, addr);
    gen_data_load(ctx, dr, addr1);
    tcg_gen_andi_tl(dr, dr, 0xffff);
    gen_update_flags(dr);

    gen_helper_print_regs(cpu_env, fmt, npc);

    return true;
}

static bool trans_LDR (DisasContext *ctx, arg_LDR *a)
{
    TCGv dr = cpu_r[a->DR];
    TCGv baser = cpu_r[a->BaseR];
    TCGv addr = tcg_temp_new_i32();
    TCGv npc = tcg_constant_tl(ctx->npc);
    TCGv_ptr fmt = tcg_constant_ptr("LDR: \n");
    
    tcg_gen_addi_tl(addr, baser, sign_extend(a->offset6, 6));
    gen_data_load(ctx, dr, addr);
    tcg_gen_andi_tl(dr, dr, 0xffff);
    gen_update_flags(dr);
    
    gen_helper_print_regs(cpu_env, fmt, npc);

    return true;
}

static bool trans_LEA (DisasContext *ctx, arg_LEA *a)
{
    TCGv dr = cpu_r[a->DR];
    TCGv addr = tcg_temp_new_i32();
    TCGv npc = tcg_constant_tl(ctx->npc);
    TCGv_ptr fmt = tcg_constant_ptr("LEA: \n");
    
    tcg_gen_movi_tl(addr, ctx->npc);
    tcg_gen_addi_tl(dr, addr, sign_extend(a->PCoffset9, 9));
    tcg_gen_andi_tl(dr, dr, 0xffff);
    gen_update_flags(dr);

    gen_helper_print_regs(cpu_env, fmt, npc);

    return true;
}

static bool trans_NOT (DisasContext *ctx, arg_NOT *a)
{
    TCGv dr = cpu_r[a->DR];
    TCGv sr = cpu_r[a->SR];
    TCGv npc = tcg_constant_tl(ctx->npc);
    TCGv_ptr fmt = tcg_constant_ptr("NOT: \n");
    
    tcg_gen_not_tl(dr, sr);
    tcg_gen_andi_tl(dr, dr, 0xffff);
    gen_update_flags(dr);

    gen_helper_print_regs(cpu_env, fmt, npc);

    return true;
}

static bool trans_RTI (DisasContext *ctx, arg_RTI *a)
{
    TCGv npc = tcg_constant_tl(ctx->npc);
    TCGv_ptr fmt = tcg_constant_ptr("RTI: \n");

    gen_helper_print_regs(cpu_env, fmt, npc);

    return true;
}

static bool trans_ST  (DisasContext *ctx, arg_ST *a)
{
    TCGv sr = cpu_r[a->SR];
    TCGv addr = tcg_temp_new_i32();
    TCGv npc = tcg_constant_tl(ctx->npc);
    TCGv_ptr fmt = tcg_constant_ptr("ST: \n");
    
    tcg_gen_movi_tl(addr, ctx->npc);
    tcg_gen_addi_tl(addr, addr, sign_extend(a->PCoffset9, 9));
    gen_data_store(ctx, sr, addr);

    gen_helper_print_regs(cpu_env, fmt, npc);

    return true;
}

static bool trans_STI (DisasContext *ctx, arg_STI *a)
{
    TCGv sr = cpu_r[a->SR];
    TCGv addr = tcg_temp_new_i32();
    TCGv addr1 = tcg_temp_new_i32();
    TCGv npc = tcg_constant_tl(ctx->npc);
    TCGv_ptr fmt = tcg_constant_ptr("STI: \n");
    
    tcg_gen_movi_tl(addr, ctx->npc);
    tcg_gen_addi_tl(addr, addr, sign_extend(a->PCoffset9, 9));
    gen_data_load(ctx, addr1, addr);
    gen_data_store(ctx, sr, addr1);

    gen_helper_print_regs(cpu_env, fmt, npc);

    return true;
}

static bool trans_STR (DisasContext *ctx, arg_STR *a)
{
    TCGv sr = cpu_r[a->SR];
    TCGv baser = cpu_r[a->BaseR];
    TCGv addr = tcg_temp_new_i32();
    TCGv npc = tcg_constant_tl(ctx->npc);
    TCGv_ptr fmt = tcg_constant_ptr("STR: \n");
    
    tcg_gen_addi_tl(addr, baser, sign_extend(a->offset6, 6));
    gen_data_store(ctx, sr, addr);

    gen_helper_print_regs(cpu_env, fmt, npc);

    return true;
}

static bool trans_TRAP(DisasContext *ctx, arg_TRAP *a)
{
    TCGv trapvect8 = tcg_constant_i32(a->trapvect8);
    TCGv npc = tcg_constant_tl(ctx->npc);
    TCGv_ptr fmt = tcg_constant_ptr("TRAP: \n");
    
    tcg_gen_movi_tl(cpu_r[7], ctx->npc);
    gen_helper_trap(cpu_env, trapvect8);

    gen_helper_print_regs(cpu_env, fmt, npc);

    return true;
}

static void translate(DisasContext *ctx)
{
    uint32_t opcode = next_word(ctx);

    if (!decode_insn(ctx, opcode)) {
        gen_helper_unsupported(cpu_env);
        ctx->base.is_jmp = DISAS_NORETURN;
    }
}

static void lc3_tr_init_disas_context(DisasContextBase *dcbase, CPUState *cs)
{
    DisasContext *ctx = container_of(dcbase, DisasContext, base);
    CPULC3State *env = cs->env_ptr;

    ctx->cs = cs;
    ctx->env = env;
    ctx->npc = ctx->base.pc_first / 2;
}

static void lc3_tr_tb_start(DisasContextBase *db, CPUState *cs)
{
}

static void lc3_tr_insn_start(DisasContextBase *dcbase, CPUState *cs)
{
    DisasContext *ctx = container_of(dcbase, DisasContext, base);

    tcg_gen_insn_start(ctx->npc);
}

static void lc3_tr_translate_insn(DisasContextBase *dcbase, CPUState *cs)
{
    DisasContext *ctx = container_of(dcbase, DisasContext, base);

    translate(ctx);

    ctx->base.pc_next = ctx->npc * 2;

    if (ctx->base.is_jmp == DISAS_NEXT) {
        target_ulong page_first = ctx->base.pc_first & TARGET_PAGE_MASK;

        if ((ctx->base.pc_next - page_first) >= TARGET_PAGE_SIZE - 4) {
            ctx->base.is_jmp = DISAS_TOO_MANY;
        }
    }
}

static void lc3_tr_tb_stop(DisasContextBase *dcbase, CPUState *cs)
{
    DisasContext *ctx = container_of(dcbase, DisasContext, base);

    switch (ctx->base.is_jmp) {
    case DISAS_NORETURN:
        break;
    case DISAS_CHAIN:
        gen_goto_tb(ctx, 1, ctx->npc);
        break;
    case DISAS_TOO_MANY:
        tcg_gen_movi_tl(cpu_pc, ctx->npc);
    case DISAS_LOOKUP:
        // 找不到会返回 tcg_code_gen_epilogue，所以不需要 exit
        // lookup_tb_ptr->cpu_get_tb_cpu_state->lc3(cpu.c) R_PC
        tcg_gen_lookup_and_goto_ptr();
        break;
    case DISAS_EXIT:
        tcg_gen_exit_tb(NULL, 0);
        break;
    default:
        g_assert_not_reached();
    }
}

static void lc3_tr_disas_log(const DisasContextBase *dcbase,
                             CPUState *cs, FILE *logfile)
{
    fprintf(logfile, "IN: %s\n", lookup_symbol(dcbase->pc_first));
    target_disas(logfile, cs, dcbase->pc_first, dcbase->tb->size);
}

static const TranslatorOps lc3_tr_ops = {
    .init_disas_context = lc3_tr_init_disas_context,
    .tb_start           = lc3_tr_tb_start,
    .insn_start         = lc3_tr_insn_start,
    .translate_insn     = lc3_tr_translate_insn,
    .tb_stop            = lc3_tr_tb_stop,
    .disas_log          = lc3_tr_disas_log,
};

void gen_intermediate_code(CPUState *cs, TranslationBlock *tb, int *max_insns,
                           target_ulong pc, void *host_pc)
{
    DisasContext dc = { };
    translator_loop(cs, tb, max_insns, pc, host_pc, &lc3_tr_ops, &dc.base);
}
