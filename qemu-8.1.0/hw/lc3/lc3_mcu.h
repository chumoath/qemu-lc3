#ifndef HW_LC3_MCU_H
#define HW_LC3_MCU_H

#include "target/lc3/cpu.h"
#include "qom/object.h"
#include "hw/sysbus.h"

#define TYPE_LC3_MCU     "lc3-mcu"

typedef struct LC3McuState LC3McuState;
DECLARE_INSTANCE_CHECKER(LC3McuState, LC3_MCU, TYPE_LC3_MCU)

struct LC3McuState {
    /*< private >*/
    SysBusDevice parent_obj;
    /*< public >*/

    LC3CPU cpu;
    MemoryRegion sram;
};

#endif /* HW_LC3_MCU_H */
