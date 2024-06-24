/*
 * QEMU ATmega MCU
 *
 * Copyright (c) 2019-2020 Philippe Mathieu-Daudé
 *
 * This work is licensed under the terms of the GNU GPLv2 or later.
 * See the COPYING file in the top-level directory.
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

#include "qemu/osdep.h"
#include "qemu/module.h"
#include "qemu/units.h"
#include "qapi/error.h"
#include "exec/memory.h"
#include "exec/address-spaces.h"
#include "sysemu/sysemu.h"
#include "hw/qdev-properties.h"
#include "hw/sysbus.h"
#include "qom/object.h"
#include "hw/misc/unimp.h"
#include "lc3_mcu.h"

struct LC3McuClass {
    /*< private >*/
    SysBusDeviceClass parent_class;
    /*< public >*/
    const char *cpu_type;
    size_t sram_size;
};

typedef struct LC3McuClass LC3McuClass;

DECLARE_CLASS_CHECKERS(LC3McuClass, LC3_MCU, TYPE_LC3_MCU)

static void lc3_realize(DeviceState *dev, Error **errp)
{
    LC3McuState *s = LC3_MCU(dev);
    const LC3McuClass *mc = LC3_MCU_GET_CLASS(dev);

    /* CPU */
    object_initialize_child(OBJECT(dev), "cpu", &s->cpu, mc->cpu_type);
    qdev_realize(DEVICE(&s->cpu), NULL, &error_abort);

    /* SRAM */
    memory_region_init_ram(&s->sram, OBJECT(dev), "sram", mc->sram_size, &error_abort);
    memory_region_add_subregion(get_system_memory(), 0, &s->sram);
}

static void lc3_class_init(ObjectClass *oc, void *data)
{
    LC3McuClass *amc = LC3_MCU_CLASS(oc);
    DeviceClass *dc = DEVICE_CLASS(oc);

    dc->realize = lc3_realize;
    amc->cpu_type = TYPE_LC3_CPU;
    #define MEMORY_MAX (1 << 17)
    amc->sram_size = MEMORY_MAX;
    #undef MEMORY_MAX
};

static const TypeInfo lc3_mcu_types[] = {
    {
        .name           = TYPE_LC3_MCU,
        .parent         = TYPE_SYS_BUS_DEVICE,
        .instance_size  = sizeof(LC3McuState),
        .class_size     = sizeof(LC3McuClass),
        .class_init     = lc3_class_init,
    }
};

DEFINE_TYPES(lc3_mcu_types)
