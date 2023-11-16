/*
 * tick_.h
 *
 *  Created on: Jul 20, 2017
 *      Author: chocolate
 */

#ifndef SRC_HARDWARE_SYSTICKINTERRUPT_TICK__H_
#define SRC_HARDWARE_SYSTICKINTERRUPT_TICK__H_

#include "base.h"
#include "xmc_ccu4.h"
#include "xmc_scu.h"

#define PinPulseHandler CCU40_0_IRQHandler
#define DelayUsHandler CCU40_1_IRQHandler

extern const XMC_CCU4_SLICE_COMPARE_CONFIG_t PinPulse_slice_config;

void Handwritten_TIMER_Init_with_params(XMC_CCU4_MODULE_t * const module,
		XMC_CCU4_SLICE_t * const slice,
		XMC_CCU4_SLICE_COMPARE_CONFIG_t * const timer_config,
		uint8_t slice_number);
void Handwritten_TIMER_Start(XMC_CCU4_SLICE_t * const slice);
void Handwritten_TIMER_Stop(XMC_CCU4_SLICE_t * const slice);
void Handwritten_TIMER_ClearEvent(XMC_CCU4_SLICE_t * const slice);
void Handwritten_TIMER_Clear(XMC_CCU4_SLICE_t * const slice);

#endif /* SRC_HARDWARE_SYSTICKINTERRUPT_TICK__H_ */
