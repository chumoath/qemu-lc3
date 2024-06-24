/*
 * QEMU LC3 CPU
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
#include "qapi/error.h"
#include "qemu/qemu-print.h"
#include "exec/exec-all.h"
#include "cpu.h"
#include "disas/dis-asm.h"
#include "tcg/debug-assert.h"

static void lc3_cpu_set_pc(CPUState *cs, vaddr value)
{
    LC3CPU *cpu = LC3_CPU(cs);

    cpu->env.R_PC = value / 2; /* internally PC points to words */
}

static vaddr lc3_cpu_get_pc(CPUState *cs)
{
    LC3CPU *cpu = LC3_CPU(cs);

    return cpu->env.R_PC * 2;
}

static bool lc3_cpu_has_work(CPUState *cs)
{
    return true;
}

static void lc3_cpu_synchronize_from_tb(CPUState *cs,
                                        const TranslationBlock *tb)
{
    LC3CPU *cpu = LC3_CPU(cs);
    CPULC3State *env = &cpu->env;

    tcg_debug_assert(!(cs->tcg_cflags & CF_PCREL));
    env->R_PC = tb->pc / 2;
}

static void lc3_restore_state_to_opc(CPUState *cs,
                                     const TranslationBlock *tb,
                                     const uint64_t *data)
{
    LC3CPU *cpu = LC3_CPU(cs);
    CPULC3State *env = &cpu->env;

    env->R_PC = data[0];
}

static void lc3_cpu_reset_hold(Object *obj)
{
    CPUState *cs = CPU(obj);
    LC3CPU *cpu = LC3_CPU(cs);
    LC3CPUClass *mcc = LC3_CPU_GET_CLASS(cpu);
    CPULC3State *env = &cpu->env;

    if (mcc->parent_phases.hold) {
        mcc->parent_phases.hold(obj);
    }

    env->R_PC = 0x3000;
    env->R_Z = 1;
    env->R_P = 0;
    env->R_N = 0;
    memset(env->r, 0, sizeof(env->r));
}

static void lc3_cpu_disas_set_info(CPUState *cpu, disassemble_info *info)
{
    info->mach = bfd_arch_lc3;
    info->print_insn = lc3_print_insn;
}

static void lc3_cpu_realizefn(DeviceState *dev, Error **errp)
{
    CPUState *cs = CPU(dev);
    LC3CPUClass *mcc = LC3_CPU_GET_CLASS(dev);
    Error *local_err = NULL;

    cpu_exec_realizefn(cs, &local_err);
    if (local_err != NULL) {
        error_propagate(errp, local_err);
        return;
    }
    qemu_init_vcpu(cs);
    cpu_reset(cs);

    mcc->parent_realize(dev, errp);
}

static void lc3_cpu_set_int(void *opaque, int irq, int level)
{

}

static void lc3_cpu_initfn(Object *obj)
{
    LC3CPU *cpu = LC3_CPU(obj);

    cpu_set_cpustate_pointers(cpu);
}

static ObjectClass *lc3_cpu_class_by_name(const char *cpu_model)
{
    ObjectClass *oc;

    oc = object_class_by_name(cpu_model);
    if (object_class_dynamic_cast(oc, TYPE_LC3_CPU) == NULL ||
        object_class_is_abstract(oc)) {
        oc = NULL;
    }
    return oc;
}

static void lc3_cpu_dump_state(CPUState *cs, FILE *f, int flags)
{
    LC3CPU *cpu = LC3_CPU(cs);
    CPULC3State *env = &cpu->env;
    int i;

    qemu_fprintf(f, "\n");
    qemu_fprintf(f, "R_PC:    %04x\n", env->R_PC);
    qemu_fprintf(f, "R_P:      %04x\n", env->R_P);
    qemu_fprintf(f, "R_Z:      %04x\n", env->R_Z);
    qemu_fprintf(f, "R_N:      %04x\n", env->R_N);
    qemu_fprintf(f, "\n");
    for (i = 0; i < ARRAY_SIZE(env->r); i++) {
        qemu_fprintf(f, "R[%02d]:  %04x   ", i, env->r[i]);
        qemu_fprintf(f, "\n");
    }
}

#include "hw/core/sysemu-cpu-ops.h"

static const struct SysemuCPUOps lc3_sysemu_ops = {
    .get_phys_page_debug = lc3_cpu_get_phys_page_debug,
};

#include "hw/core/tcg-cpu-ops.h"

static const struct TCGCPUOps lc3_tcg_ops = {
    .initialize = lc3_cpu_tcg_init,
    .synchronize_from_tb = lc3_cpu_synchronize_from_tb,
    .restore_state_to_opc = lc3_restore_state_to_opc,
    .cpu_exec_interrupt = lc3_cpu_exec_interrupt,
    .tlb_fill = lc3_cpu_tlb_fill,
    .do_interrupt = lc3_cpu_do_interrupt,
};

static void lc3_cpu_class_init(ObjectClass *oc, void *data)
{
    DeviceClass *dc = DEVICE_CLASS(oc);
    CPUClass *cc = CPU_CLASS(oc);
    LC3CPUClass *mcc = LC3_CPU_CLASS(oc);
    ResettableClass *rc = RESETTABLE_CLASS(oc);

    device_class_set_parent_realize(dc, lc3_cpu_realizefn, &mcc->parent_realize);

    resettable_class_set_parent_phases(rc, NULL, lc3_cpu_reset_hold, NULL,
                                       &mcc->parent_phases);

    cc->class_by_name = lc3_cpu_class_by_name;

    cc->has_work = lc3_cpu_has_work;
    cc->dump_state = lc3_cpu_dump_state;
    cc->set_pc = lc3_cpu_set_pc;
    cc->get_pc = lc3_cpu_get_pc;
    dc->vmsd = &vms_lc3_cpu;
    cc->sysemu_ops = &lc3_sysemu_ops;
    cc->disas_set_info = lc3_cpu_disas_set_info;
    cc->gdb_read_register = lc3_cpu_gdb_read_register;
    cc->gdb_write_register = lc3_cpu_gdb_write_register;
    cc->gdb_adjust_breakpoint = lc3_cpu_gdb_adjust_breakpoint;
    cc->gdb_num_core_regs = 35;
    cc->gdb_core_xml_file = "lc3-cpu.xml";
    cc->tcg_ops = &lc3_tcg_ops;
}

static void lc3_cpu_list_entry(gpointer data, gpointer user_data)
{
    const char *typename = object_class_get_name(OBJECT_CLASS(data));

    qemu_printf("%s\n", typename);
}

void lc3_cpu_list(void)
{
    GSList *list;
    list = object_class_get_list_sorted(TYPE_LC3_CPU, false);
    g_slist_foreach(list, lc3_cpu_list_entry, NULL);
    g_slist_free(list);
}

static const TypeInfo lc3_cpu_type_info[] = {
    {
        .name = TYPE_LC3_CPU,
        .parent = TYPE_CPU,
        .instance_size = sizeof(LC3CPU),
        .instance_init = lc3_cpu_initfn,
        .class_size = sizeof(LC3CPUClass),
        .class_init = lc3_cpu_class_init,
    }
};

DEFINE_TYPES(lc3_cpu_type_info)
