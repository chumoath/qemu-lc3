#include "qemu/osdep.h"
#include "qapi/error.h"
#include "lc3_mcu.h"
#include "boot.h"
#include "qom/object.h"

struct LC3MachineState {
    /*< private >*/
    MachineState parent_obj;
    /*< public >*/
    LC3McuState mcu;
};

typedef struct LC3MachineState LC3MachineState;

struct LC3MachineClass {
    /*< private >*/
    MachineClass parent_class;
    /*< public >*/
    const char *mcu_type;
};

typedef struct LC3MachineClass LC3MachineClass;

#define TYPE_LC3_MACHINE MACHINE_TYPE_NAME("lc3-machine")

DECLARE_OBJ_CHECKERS(LC3MachineState, LC3MachineClass, LC3_MACHINE, TYPE_LC3_MACHINE)

static void lc3_machine_init(MachineState *machine)
{
    LC3MachineClass *amc = LC3_MACHINE_GET_CLASS(machine);
    LC3MachineState *ams = LC3_MACHINE(machine);

    object_initialize_child(OBJECT(machine), "mcu", &ams->mcu, amc->mcu_type);
    sysbus_realize(SYS_BUS_DEVICE(&ams->mcu), &error_abort);

    if (machine->firmware) {
        if (!lc3_load_firmware(&ams->mcu.cpu, machine, &ams->mcu.sram, machine->firmware)) {
            exit(1);
        }
    }
}

static void lc3_machine_class_init(ObjectClass *oc, void *data)
{
    MachineClass *mc = MACHINE_CLASS(oc);
    LC3MachineClass *amc = LC3_MACHINE_CLASS(oc);

    mc->init = lc3_machine_init;
    mc->desc        = "LC3 Machine";
    mc->alias       = "LC3 Board";
    amc->mcu_type   = TYPE_LC3_MCU;
};

static const TypeInfo lc3_machine_types[] = {
    {
        .name           = TYPE_LC3_MACHINE,
        .parent         = TYPE_MACHINE,
        .instance_size  = sizeof(LC3MachineState),
        .class_size     = sizeof(LC3MachineClass),
        .class_init     = lc3_machine_class_init,
    }
};

DEFINE_TYPES(lc3_machine_types)
