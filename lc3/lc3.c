#include <stdio.h>
#include <stdint.h>
#include <signal.h>
/* unix only */
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/termios.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdarg.h>


#define MEMORY_MAX (1 << 16)
uint16_t memory[MEMORY_MAX];  /* 65536 locations */

enum
{
    R_R0 = 0,
    R_R1,
    R_R2,
    R_R3,
    R_R4,
    R_R5,
    R_R6,
    R_R7,
    R_PC, /* program counter */
    R_COND,
    R_COUNT
};

uint16_t reg[R_COUNT];

enum
{
    OP_BR = 0, /* branch */
    OP_ADD,    /* add  */
    OP_LD,     /* load */
    OP_ST,     /* store */
    OP_JSR,    /* jump register */
    OP_AND,    /* bitwise and */
    OP_LDR,    /* load register */
    OP_STR,    /* store register */
    OP_RTI,    /* unused */
    OP_NOT,    /* bitwise not */
    OP_LDI,    /* load indirect */
    OP_STI,    /* store indirect */
    OP_JMP,    /* jump */
    OP_RES,    /* reserved (unused) */
    OP_LEA,    /* load effective address */
    OP_TRAP    /* execute trap */
};

enum
{
    FL_POS = 1 << 0, /* P */
    FL_ZRO = 1 << 1, /* Z */
    FL_NEG = 1 << 2, /* N */
};

enum
{
    MR_KBSR = 0xFE00, /* keyboard status */
    MR_KBDR = 0xFE02  /* keyboard data */
};

enum
{
    TRAP_GETC = 0x20,  /* get character from keyboard, not echoed onto the terminal */
    TRAP_OUT = 0x21,   /* output a character */
    TRAP_PUTS = 0x22,  /* output a word string */
    TRAP_IN = 0x23,    /* get character from keyboard, echoed onto the terminal */
    TRAP_PUTSP = 0x24, /* output a byte string */
    TRAP_HALT = 0x25   /* halt the program */
};

struct termios original_tio;

#ifdef LC3_LOG
static void lc3_log(const char *log_file_name, const char *fmt, ...)
{
    char log_file_path[64], mode[2] = "a";
    sprintf (log_file_path, "%s", log_file_name);

    #if 0
    struct stat st = {};
    stat (log_file_path, &st);
    if (st.st_size > 200 * 1024) {
        mode[0] = 'w';
    }
    #endif

    FILE *fp = NULL;
    fp = fopen (log_file_path, mode);
    
    va_list args;
    va_start (args, fmt);
    vfprintf (fp, fmt, args);
    fflush(fp);
    va_end (args);
    fclose (fp);
}

void helper_print_regs(const void *fmt, uint32_t npc)
{
    lc3_log("debug.txt", fmt);
    lc3_log("debug.txt", "    npc: 0x%x\n", npc);
    lc3_log("debug.txt", "    R_P: 0x%x R_Z: 0x%x R_N: 0x%x\n", reg[R_COND] == FL_POS, reg[R_COND] == FL_ZRO, reg[R_COND] == FL_NEG);
    lc3_log("debug.txt", "    ");
    for (int i = 0; i < 8; i++) {
        lc3_log("debug.txt", "R%d: 0x%x ", i, reg[i]);
    }
    lc3_log("debug.txt", "\n\n");
}
#else
void helper_print_regs(const void *fmt, uint32_t npc)
{
}
#endif


void disable_input_buffering()
{
    tcgetattr(STDIN_FILENO, &original_tio);
    struct termios new_tio = original_tio;
    new_tio.c_lflag &= ~ICANON & ~ECHO;
    tcsetattr(STDIN_FILENO, TCSANOW, &new_tio);
}

void restore_input_buffering()
{
    tcsetattr(STDIN_FILENO, TCSANOW, &original_tio);
}

uint16_t check_key()
{
    fd_set readfds;
    FD_ZERO(&readfds);
    FD_SET(STDIN_FILENO, &readfds);

    struct timeval timeout;
    timeout.tv_sec = 0;
    timeout.tv_usec = 0;
    return select(1, &readfds, NULL, NULL, &timeout) != 0;
}

void handle_interrupt(int signal)
{
    restore_input_buffering();
    printf("\n");
    exit(-2);
}

uint16_t swap16(uint16_t x)
{
    return (x << 8) | (x >> 8);
}

void read_image_file(FILE* file)
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

int read_image(const char* image_path)
{
    FILE* file = fopen(image_path, "rb");
    if (!file) { return 0; };
    read_image_file(file);
    fclose(file);
    return 1;
}

void mem_write(uint16_t address, uint16_t val)
{
    memory[address] = val;
}

uint16_t mem_read(uint16_t address)
{
    if (address == MR_KBSR)
    {
        if (check_key())
        {
            memory[MR_KBSR] = (1 << 15);
            memory[MR_KBDR] = getchar();
        }
        else
        {
            memory[MR_KBSR] = 0;
        }
    }
    return memory[address];
}

// 只能使用 32bit 扩展，16bit扩展无效，还是当作32bit
// 搞16bit扩展成什么无所谓
int16_t sig_extend(uint16_t val, uint16_t bit, uint16_t len) {
    return (((int32_t)val) << (15 - bit + 16)) >> (16 - len + 16);
}

uint16_t zero_extend(uint16_t val, uint16_t bit, uint16_t len) {
    return (((uint32_t)val) << (15 - bit + 16)) >> (16 - len + 16);
}


uint16_t sign_extend(uint16_t x, int bit_count)
{
    if ((x >> (bit_count - 1)) & 1) {
        x |= (0xFFFF << bit_count);
    }
    return x;
}

void update_flags(uint16_t r)
{
    if (reg[r] == 0)
    {
        reg[R_COND] = FL_ZRO;
    }
    else if (reg[r] >> 15) /* a 1 in the left-most bit indicates negative */
    {
        reg[R_COND] = FL_NEG;
    }
    else
    {
        reg[R_COND] = FL_POS;
    }
}

#define DR(instruction)                   ((instruction & 0x0E00) >> 9)
#define SR(instruction)                   ((instruction & 0x0E00) >> 9)
#define SR1(instruction)                  ((instruction & 0x01C0) >> 6)
#define SR2(instruction)                  (instruction & 0x7)
#define Imm5(instruction)                 (sign_extend(instruction & 0x1F, 5))
#define BaseR(instruction)                ((instruction & 0x01C0) >> 6)
#define PCoffset11(instruction)           (sign_extend(instruction & 0x07FF, 11))
#define PCoffset9(instruction)            (sign_extend(instruction & 0x01FF, 9))
#define offset6(instruction)              (sign_extend(instruction & 0x003F, 6))
#define trapvect8(instruction)            (instruction & 0x00FF)
#define fn(instruction)                   (instruction & 0x0800)
#define fz(instruction)                   (instruction & 0x0400)
#define fp(instruction)                   (instruction & 0x0200)



int main(int argc, const char* argv[])
{
    if (argc < 2)
    {
        /* show usage string */
        printf("lc3 [image-file1] ...\n");
        exit(2);
    }

    for (int j = 1; j < argc; ++j)
    {
        if (!read_image(argv[j]))
        {
            printf("failed to load image: %s\n", argv[j]);
            exit(1);
        }
    }

    signal(SIGINT, handle_interrupt);
    disable_input_buffering();

    /* since exactly one condition flag should be set at any given time, set the Z flag */
    reg[R_COND] = FL_ZRO;

    /* set the PC to starting position */
    /* 0x3000 is the default */
    enum { PC_START = 0x3000 };
    reg[R_PC] = PC_START;

    int running = 1;
    while (running)
    {
        /* FETCH */
        uint16_t instr = mem_read(reg[R_PC]++);
        uint16_t op = instr >> 12;
        switch (op)
        {
            case OP_ADD:
                if (instr & 0x0020) {
                    reg[DR(instr)] = (reg[SR1(instr)] + Imm5(instr)) & 0xFFFF;
                } else {
                    reg[DR(instr)] = (reg[SR1(instr)] + reg[SR2(instr)]) & 0xFFFF;
                }
                update_flags(DR(instr));

                if (instr & 0x0020) {
                    helper_print_regs("ADDI: \n", reg[R_PC]);
                } else {
                    helper_print_regs("ADD: \n", reg[R_PC]);
                }
                break;
            case OP_AND:
                if (instr & 0x0020) {
                    reg[DR(instr)] = (reg[SR1(instr)] & Imm5(instr)) & 0xFFFF;
                } else {
                    reg[DR(instr)] = (reg[SR1(instr)] & reg[SR2(instr)]) & 0xFFFF;
                }
                update_flags(DR(instr));
                
                if (instr & 0x0020) {
                    helper_print_regs("ANDI: \n", reg[R_PC]);
                } else {
                    helper_print_regs("AND: \n", reg[R_PC]);
                }
                break;
            case OP_NOT:
                reg[DR(instr)] = (~reg[SR(instr)]) & 0xFFFF;
                update_flags(DR(instr));
                helper_print_regs("NOT: \n", reg[R_PC]);
                break;
            case OP_BR:
                helper_print_regs("BR: \n", reg[R_PC]);
                
                if ((fn(instr) && reg[R_COND] == FL_NEG) || (fz(instr) && reg[R_COND] == FL_ZRO) || (fp(instr) && reg[R_COND] == FL_POS)) {
                    helper_print_regs("BR: 1\n", reg[R_PC]);
                    reg[R_PC] = (reg[R_PC] + PCoffset9(instr)) & 0xFFFF;
                } else {
                    helper_print_regs("BR: 0\n", reg[R_PC]);
                }
                break;
            case OP_JMP:
                helper_print_regs("JMP: \n", reg[R_PC]);
                reg[R_PC] = reg[BaseR(instr)] & 0xFFFF;
                break;
            case OP_JSR:
                reg[R_R7] = reg[R_PC];
                if (instr & 0x0800) {
                    helper_print_regs("JSR: \n", reg[R_PC]);
                    reg[R_PC] = (reg[R_PC] + PCoffset11(instr)) & 0xFFFF;
                } else {
                    helper_print_regs("JSRR: \n", reg[R_PC]);
                    reg[R_PC] = reg[BaseR(instr)];
                }
                break;
            case OP_LD:
                reg[DR(instr)] = mem_read((reg[R_PC] + PCoffset9(instr)) & 0xFFFF);
                update_flags(DR(instr));
                helper_print_regs("LD: \n", reg[R_PC]);

                break;
            case OP_LDI:
                reg[DR(instr)] = mem_read(mem_read((reg[R_PC] + PCoffset9(instr)) & 0xFFFF));
                update_flags(DR(instr));
                helper_print_regs("LDI: \n", reg[R_PC]);

                break;
            case OP_LDR:
                reg[DR(instr)] = mem_read((reg[BaseR(instr)] + offset6(instr)) & 0xFFFF);
                update_flags(DR(instr));
                helper_print_regs("LDR: \n", reg[R_PC]);

                break;
            case OP_LEA:
                reg[DR(instr)] = (reg[R_PC] + PCoffset9(instr)) & 0xFFFF;
                update_flags(DR(instr));
                helper_print_regs("LEA: \n", reg[R_PC]);

                break;
            case OP_ST:
                mem_write(((reg[R_PC] + PCoffset9(instr)) & 0xFFFF), reg[SR(instr)]);
                helper_print_regs("ST: \n", reg[R_PC]);

                break;
            case OP_STI:
                mem_write(mem_read((reg[R_PC] + PCoffset9(instr)) & 0xFFFF), reg[SR(instr)]);
                helper_print_regs("STI: \n", reg[R_PC]);

                break;
            case OP_STR:
                mem_write((reg[BaseR(instr)] + offset6(instr)) & 0xFFFF, reg[SR(instr)]);
                helper_print_regs("STR: \n", reg[R_PC]);

                break;
            case OP_TRAP:
                reg[R_R7] = reg[R_PC];
                switch (trapvect8(instr)) {
                    case TRAP_GETC: 
                    {
                        /* read a single ASCII char */
                        reg[R_R0] = (uint16_t)getchar();
                        update_flags(R_R0);
                        break;
                    }
                    case TRAP_OUT:
                    {
                        putc((char)reg[R_R0], stdout);
                        fflush(stdout);
                        break;
                    }
                    case TRAP_PUTS:
                    {
                        /* one char per word */
                        uint16_t* c = memory + reg[R_R0];
                        while (*c)
                        {
                            putc((char)*c, stdout);
                            ++c;
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
                        reg[R_R0] = (uint16_t)c;
                        update_flags(R_R0);
                        break;
                    }
                    case TRAP_PUTSP:
                    {
                        /* one char per byte (two bytes per word)
                        here we need to swap back to
                        big endian format */
                        uint16_t* c = memory + reg[R_R0];
                        while (*c)
                        {
                            char char1 = (*c) & 0xFF;
                            putc(char1, stdout);
                            char char2 = (*c) >> 8;
                            if (char2) putc(char2, stdout);
                            ++c;
                        }
                        fflush(stdout);
                        break;
                    }
                    case TRAP_HALT:
                    {
                        puts("HALT");
                        fflush(stdout);
                        running = 0;
                        break;
                    }
                    case OP_RES:
                    case OP_RTI:
                    default:
                        exit(-2);
                        break;
                }
                helper_print_regs("TRAP: \n", reg[R_PC]);
        }
    }

    restore_input_buffering();
}
