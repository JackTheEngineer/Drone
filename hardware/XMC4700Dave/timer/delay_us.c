/*
 * delay_us.c
 *
 *  Created on: Nov 14, 2017
 *      Author: chocolate
 */

#include "delay_us.h"
#include "xmc_ccu4.h"
#include "timer.h"


static XMC_CCU4_MODULE_t * const module  = (XMC_CCU4_MODULE_t*) CCU40;
static XMC_CCU4_SLICE_t * const slice = (XMC_CCU4_SLICE_t*) CCU40_CC41;

static volatile uint8_t time_passed;

void DelayTimer_Init(void){
	Handwritten_TIMER_Init_with_params(module,
			slice,
			(XMC_CCU4_SLICE_COMPARE_CONFIG_t * const)&PinPulse_slice_config,
			1);
	NVIC_SetPriority(CCU40_1_IRQn,
			NVIC_EncodePriority(NVIC_GetPriorityGrouping(),
							62, /* Level 0 is the highest level */
							0)); /* Level 0 is the highest level */
	NVIC_EnableIRQ(CCU40_1_IRQn);
}
void _delay_us(uint16_t us){
	XMC_CCU4_SLICE_SetPrescaler(slice, 4);
	XMC_CCU4_SLICE_SetTimerPeriodMatch(slice, 9*us);
	XMC_CCU4_SLICE_SetTimerCompareMatch(slice, (uint16_t)0);
	XMC_CCU4_SLICE_ClearTimer(slice);
	XMC_CCU4_EnableShadowTransfer(module, (uint32_t)
			((uint32_t)XMC_CCU4_SHADOW_TRANSFER_SLICE_1 |
			(uint32_t)XMC_CCU4_SHADOW_TRANSFER_PRESCALER_SLICE_1));
	time_passed = 0;
	Handwritten_TIMER_Start(slice);
	while(time_passed == 0){}
}

void DelayUsHandler(void){
	Handwritten_TIMER_ClearEvent(slice);
	time_passed = 1;
	Handwritten_TIMER_Stop(slice);
}
