#include "qemu/osdep.h"
#include "qemu/datadir.h"
#include "hw/loader.h"
#include "elf.h"
#include "boot.h"
#include "qemu/error-report.h"

#define MEMORY_MAX (1 << 16)
static uint16_t memory[MEMORY_MAX];  /* 65536 locations */

static uint16_t swap16(uint16_t x)
{
    return (x << 8) | (x >> 8);
}

static void read_image_file(FILE* file)
{
    /* the origin tells us where in memory to place the image */
    uint16_t origin;
    fread(&origin, sizeof(origin), 1, file);
    origin = swap16(origin);

    /* we know the maximum file size so we only need one fread */
    uint16_t max_read = MEMORY_MAX - origin;
    uint16_t* p = memory + origin;
    size_t read = fread(p, sizeof(uint16_t), max_read, file);

    /* swap to little endian */
    while (read-- > 0)
    {
        *p = swap16(*p);
        ++p;
    }
}

static int read_image(const char* image_path)
{
    FILE* file = fopen(image_path, "rb");
    if (!file) { return 0; };
    read_image_file(file);
    fclose(file);
    return 1;
}

bool lc3_load_firmware(LC3CPU *cpu, MachineState *ms, MemoryRegion *program_mr, const char *firmware)
{
    g_autofree char *filename = NULL;
    int read_ret;
    uint16_t val;

    filename = qemu_find_file(QEMU_FILE_TYPE_BIOS, firmware);
    if (filename == NULL) {
        error_report("Unable to find %s", firmware);
        return false;
    }

    read_ret = read_image(filename);
    if (read_ret != 1) {
        error_report("Unable to read %s", filename);
        return false;
    }

    address_space_write(&address_space_memory, 0x6000, MEMTXATTRS_UNSPECIFIED, (uint8_t *)memory + 0x6000, MEMORY_MAX * 2 - 0x6000);
    address_space_read(&address_space_memory, 0x6000, MEMTXATTRS_UNSPECIFIED, &val, 2);
    return true;
}

#undef MEMORY_MAX
