#include "qemu/osdep.h"
#include "gdbstub/helpers.h"

int lc3_cpu_gdb_read_register(CPUState *cs, GByteArray *mem_buf, int n)
{
    LC3CPU *cpu = LC3_CPU(cs);
    CPULC3State *env = &cpu->env;

    /*  R */
    if (n < 8) {
        return gdb_get_reg16(mem_buf, env->r[n]);
    }

    /*  PC */
    if (n == 8) {
        return gdb_get_reg16(mem_buf, env->R_PC);
    }

    /*  COND */
    if (n == 9) {
        return gdb_get_reg16(mem_buf, env->R_P);
    }

    return 0;
}

int lc3_cpu_gdb_write_register(CPUState *cs, uint8_t *mem_buf, int n)
{
    LC3CPU *cpu = LC3_CPU(cs);
    CPULC3State *env = &cpu->env;

    /*  R */
    if (n < 8) {
        env->r[n] = lduw_p(mem_buf);
        return 2;
    }

    /*  PC */
    if (n == 8) {
        env->R_PC = lduw_p(mem_buf);
        return 2;
    }

    /*  COND */
    if (n == 9) {
        env->R_PC = lduw_p(mem_buf);
        return 2;
    }
    
    return 0;
}

vaddr lc3_cpu_gdb_adjust_breakpoint(CPUState *cpu, vaddr addr)
{
    return addr;
}
