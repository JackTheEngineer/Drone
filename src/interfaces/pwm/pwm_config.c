/*
 * pwm_config.c
 *
 *  Created on: Apr 30, 2016
 *      Author: jakov
 */

#include "../../interfaces/pwm/pwm.h"

//XMC Capture/Compare Unit 4 (CCU4) Configuration:
XMC_CCU4_SLICE_COMPARE_CONFIG_t SLICE0_config =
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

XMC_GPIO_CONFIG_t SLICE0_OUTPUT_config =
{
   .mode = XMC_GPIO_MODE_OUTPUT_PUSH_PULL_ALT4,
   .output_level = XMC_GPIO_OUTPUT_LEVEL_LOW,
   .output_strength = XMC_GPIO_OUTPUT_STRENGTH_MEDIUM
};
