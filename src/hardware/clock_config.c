/*
 * clock_config.c
 *
 *  Created on: Jul 11, 2017
 *      Author: chocolate
 */

#include "xmc4_scu.h"


//XMC System Clock Unit (SCU) Configuration:
//PWM period is calculated based on PCLK which is equivalent to 64 MHz.
XMC_SCU_CLOCK_CONFIG_t clock_config =
{
  .pclk_src = XMC_SCU_CLOCK_PCLKSRC_DOUBLE_MCLK,
  .rtc_src = XMC_SCU_CLOCK_RTCCLKSRC_DCO2,
  .fdiv = 0,
  .idiv = 1,
};
