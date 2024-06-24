#ifndef TARGET_LC3_CPU_QOM_H
#define TARGET_LC3_CPU_QOM_H

#include "hw/core/cpu.h"
#include "qom/object.h"

#define TYPE_LC3_CPU "lc3-cpu"

OBJECT_DECLARE_CPU_TYPE(LC3CPU, LC3CPUClass, LC3_CPU)

struct LC3CPUClass {
    /*< private >*/
    CPUClass parent_class;
    /*< public >*/
    DeviceRealize parent_realize;
    ResettablePhases parent_phases;
};


#endif /* TARGET_LC3_CPU_QOM_H */
