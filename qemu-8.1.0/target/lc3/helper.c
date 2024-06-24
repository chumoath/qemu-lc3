#include "qemu/osdep.h"
#include "qemu/log.h"
#include "qemu/error-report.h"
#include "cpu.h"
#include "hw/core/tcg-cpu-ops.h"
#include "exec/exec-all.h"
#include "exec/address-spaces.h"
#include "exec/helper-proto.h"

bool lc3_cpu_exec_interrupt(CPUState *cs, int interrupt_request)
{
    return true;
}

void lc3_cpu_do_interrupt(CPUState *cs)
{

}

hwaddr lc3_cpu_get_phys_page_debug(CPUState *cs, vaddr addr)
{
    return addr;
}

bool lc3_cpu_tlb_fill(CPUState *cs, vaddr address, int size,
                      MMUAccessType access_type, int mmu_idx,
                      bool probe, uintptr_t retaddr)
{
    int prot, page_size = TARGET_PAGE_SIZE;
    uint32_t paddr;

    address &= TARGET_PAGE_MASK;

    paddr = address;
    prot = PAGE_READ | PAGE_EXEC | PAGE_WRITE;
    tlb_set_page(cs, address, paddr, prot, mmu_idx, page_size);
    return true;
}

void helper_unsupported(CPULC3State *env)
{
    CPUState *cs = env_cpu(env);

    /*
     *  I count not find what happens on the real platform, so
     *  it's EXCP_DEBUG for meanwhile
     */
    cs->exception_index = EXCP_DEBUG;
    if (qemu_loglevel_mask(LOG_UNIMP)) {
        qemu_log("UNSUPPORTED\n");
        cpu_dump_state(cs, stderr, 0);
    }
    cpu_loop_exit(cs);
}

static uint16_t check_key(void)
{
    fd_set readfds;
    struct timeval timeout;

    FD_ZERO(&readfds);
    FD_SET(STDIN_FILENO, &readfds);

    timeout.tv_sec = 0;
    timeout.tv_usec = 0;
    return select(1, &readfds, NULL, NULL, &timeout) != 0;
}

target_ulong helper_fullrd(CPULC3State *env, uint32_t addr)
{
    uint16_t data, dr, sr;
    if (addr == MR_KBSR)
    {
        if (check_key())
        {
            sr = (1 << 15);
            dr = getchar();
        }
        else
        {
            sr = 0;
        }

        address_space_stw(&address_space_memory, MR_KBSR * 2, sr & 0xffff, MEMTXATTRS_UNSPECIFIED, NULL);
        address_space_stw(&address_space_memory, MR_KBDR * 2, dr & 0xffff, MEMTXATTRS_UNSPECIFIED, NULL);
    }
    data = address_space_lduw(&address_space_memory, addr * 2, MEMTXATTRS_UNSPECIFIED, NULL);
    
    return data;
}

void helper_fullwr(CPULC3State *env, uint32_t data, uint32_t addr)
{
    address_space_stw(&address_space_memory, addr * 2, data & 0xffff, MEMTXATTRS_UNSPECIFIED, NULL);
}

static void update_flags(CPULC3State *env, uint16_t r)
{
    env->R_Z = 0;
    env->R_P = 0;
    env->R_N = 0;

    if (env->r[r] == 0)
    {
        env->R_Z = 1;
    }
    else if (env->r[r] >> 15)
    {
        env->R_N = 1;
    }
    else
    {
        env->R_P = 1;
    }
}

void helper_trap(CPULC3State *env, uint32_t trapvect8)
{
    switch (trapvect8) {
        case TRAP_GETC: 
        {
            env->r[0] = (uint16_t)getchar();
            update_flags(env, 0);
            break;
        }
        case TRAP_OUT:
        {
            putc((char)env->r[0], stdout);
            fflush(stdout);
            break;
        }
        case TRAP_PUTS:
        {
            uint16_t c;
            uint16_t idx;
            idx = env->r[0];

            while (1)
            {
                c = address_space_lduw(&address_space_memory, idx++ * 2, MEMTXATTRS_UNSPECIFIED, NULL);
                if (!c) break;
                putc((char)c, stdout);
            }
            fflush(stdout);
            break;
        }
        case TRAP_IN:
        {
            printf("Enter a character: ");
            char c = getchar();
            putc(c, stdout);
            fflush(stdout);
            env->r[0] = (uint16_t)c;
            update_flags(env, 0);
            break;
        }
        case TRAP_PUTSP:
        {
            uint16_t c;
            uint16_t idx;
            idx = env->r[0];
            
            while (1)
            {
                char char1, char2;
                c = address_space_lduw(&address_space_memory, idx++ * 2, MEMTXATTRS_UNSPECIFIED, NULL);
                if (!c) break;
                char1 = (c) & 0xFF;
                putc(char1, stdout);
                
                char2 = (c) >> 8;
                if (char2) putc(char2, stdout);

            }
            fflush(stdout);
            break;
        }
        case TRAP_HALT:
        {
            puts("HALT");
            fflush(stdout);
            break;
        }
    }
}
