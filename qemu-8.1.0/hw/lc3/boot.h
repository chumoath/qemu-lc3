#ifndef HW_LC3_BOOT_H
#define HW_LC3_BOOT_H

#include "hw/boards.h"
#include "cpu.h"

bool lc3_load_firmware(LC3CPU *cpu, MachineState *ms, MemoryRegion *mr, const char *firmware);

#endif
