#include <stdio.h>
#include <stdint.h>

int16_t sig_extend(uint16_t val, uint16_t bit, uint16_t len) {
    return (((int32_t)val) << (15 - bit + 16)) >> (16 - len + 16);
}

uint16_t zero_extend(uint16_t val, uint16_t bit, uint16_t len) {
    return (((uint32_t)val) << (15 - bit + 16)) >> (16 - len + 16);
}

int main(void)
{
    uint16_t val = 0x1504;
    int16_t ret_i;
    uint16_t ret_u;

    ret_i = sig_extend(val, 10, 5);
    printf (" sig_extend = 0x%x\n", ret_i);
    ret_u = zero_extend(val, 10, 5);
    printf ("zero_extend = 0x%x\n", ret_u);

    return 0;
}