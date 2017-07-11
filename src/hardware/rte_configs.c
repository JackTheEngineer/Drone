/*
 * configs.c
 *
 *  Created on: Jul 11, 2017
 *      Author: chocolate
 */

#include "xmc_ccu4.h"
#include "xmc_gpio.h"
#include "xmc4_scu.h"

//XMC Capture/Compare Unit 4 (CCU4) Configuration:
XMC_CCU4_SLICE_COMPARE_CONFIG_t CU41_SLICE0_config =
{
  .timer_mode = (uint32_t) XMC_CCU4_SLICE_TIMER_COUNT_MODE_EA,
  .monoshot = (uint32_t) false,
  .shadow_xfer_clear = (uint32_t) 0,
  .dither_timer_period = (uint32_t) 0,
  .dither_duty_cycle = (uint32_t) 0,
  .prescaler_mode = (uint32_t) XMC_CCU4_SLICE_PRESCALER_MODE_NORMAL,
  .mcm_enable = (uint32_t) 0,
  .prescaler_initval = (uint32_t) 10, 	/* in this case, prescaler = 2^10 */
  .float_limit = (uint32_t) 0,
  .dither_limit = (uint32_t) 0,
  .passive_level = (uint32_t) XMC_CCU4_SLICE_OUTPUT_PASSIVE_LEVEL_LOW,
  .timer_concatenation = (uint32_t) 0
};

XMC_CCU4_SLICE_EVENT_CONFIG_t CU41_SLICE0_event0_config =
{
  .mapped_input = XMC_CCU4_SLICE_INPUT_I, 	/* mapped to SCU.GSC40 */
  .edge = XMC_CCU4_SLICE_EVENT_EDGE_SENSITIVITY_RISING_EDGE,
  .level = XMC_CCU4_SLICE_EVENT_LEVEL_SENSITIVITY_ACTIVE_HIGH,
  .duration = XMC_CCU4_SLICE_EVENT_FILTER_3_CYCLES
};
