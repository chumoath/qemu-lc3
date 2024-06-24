#ifndef LC3_CPU_PARAM_H
#define LC3_CPU_PARAM_H

#define TARGET_LONG_BITS 32
/*
 * TARGET_PAGE_BITS cannot be more than 8 bits because
 * 1.  all IO registers occupy [0x0000 .. 0x00ff] address range, and they
 *     should be implemented as a device and not memory
 * 2.  SRAM starts at the address 0x0100
 */
#define TARGET_PAGE_BITS 8
#define TARGET_PHYS_ADDR_SPACE_BITS 24
#define TARGET_VIRT_ADDR_SPACE_BITS 24

#endif
