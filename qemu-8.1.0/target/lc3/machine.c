#include "qemu/osdep.h"
#include "cpu.h"
#include "migration/cpu.h"


const VMStateDescription vms_lc3_cpu = {
    .name = "cpu",
    .version_id = 1,
    .minimum_version_id = 1,
    .fields = (VMStateField[]) {
        VMSTATE_UINT32(env.R_PC, LC3CPU),
        VMSTATE_UINT32(env.R_P, LC3CPU),
        VMSTATE_UINT32(env.R_N, LC3CPU),
        VMSTATE_UINT32(env.R_Z, LC3CPU),
        VMSTATE_UINT32_ARRAY(env.r, LC3CPU, NUMBER_OF_CPU_REGISTERS),
        VMSTATE_END_OF_LIST()
    }
};
