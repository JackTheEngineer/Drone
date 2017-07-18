/*
 * configs.c
 *
 *  Created on: Jul 11, 2017
 *      Author: chocolate
 */

#include "xmc_ccu4.h"
#include "xmc_gpio.h"

//XMC Capture/Compare Unit 4 (CCU4) Configuration:
const XMC_CCU4_SLICE_COMPARE_CONFIG_t CU41_SLICE1_config =
{
  .timer_mode = (uint32_t) XMC_CCU4_SLICE_TIMER_COUNT_MODE_EA,
  .monoshot = XMC_CCU4_SLICE_TIMER_REPEAT_MODE_REPEAT,
  .shadow_xfer_clear = false,
  .dither_timer_period = false,
  .dither_duty_cycle = false,
  .prescaler_mode = (uint32_t) XMC_CCU4_SLICE_PRESCALER_MODE_NORMAL,
  .mcm_enable = false,
  .prescaler_initval = (uint32_t) 4, 	/* in this case, prescaler = 2^10 */
  .float_limit = 0U,
  .dither_limit = 0U,
  .passive_level = XMC_CCU4_SLICE_OUTPUT_PASSIVE_LEVEL_LOW,
  .timer_concatenation = false,
};
