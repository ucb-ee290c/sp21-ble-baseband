#include "baseband.h"
#include "mmio.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

int main(void)
{
  printf("Pushing a channel index configure\n");

  reg_write32(BASEBAND_ADDITIONAL_DATA, 5);
  reg_write32(BASEBAND_INST, BASEBAND_INSTRUCTION(BASEBAND_CONFIG, BASEBAND_CONFIG_CHANNEL_INDEX, 0));

  printf("Done configuring\n");

  return 0;
}