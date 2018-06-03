/*
 * pin_pulse.c
 *
 *  Created on: Oct 31, 2017
 *      Author: chocolate
 */


#include "pin_pulse.h"
#include "xmc_gpio.h"
#include "xmc_scu.h"
#include "timer.h"

XMC_CCU4_MODULE_t * const module = (XMC_CCU4_MODULE_t*) CCU41;
XMC_CCU4_SLICE_t * const slice  = (XMC_CCU4_SLICE_t*) CCU41_CC41;

XMC_GPIO_CONFIG_t const gpio_config = {
		.mode = XMC_GPIO_MODE_OUTPUT_PUSH_PULL,
		.output_level = XMC_GPIO_OUTPUT_LEVEL_LOW,
		.output_strength = XMC_GPIO_OUTPUT_STRENGTH_MEDIUM,
};

void PinPulse_Init(void){
	XMC_GPIO_Init(CE_PORT, CE_PIN, &gpio_config);
	TIMER_Init_with_params(module,
			slice,
			(XMC_CCU4_SLICE_COMPARE_CONFIG_t*const)&PinPulse_slice_config,
			1); /* slice number */
	NVIC_SetPriority(CCU41_1_IRQn,
			NVIC_EncodePriority(NVIC_GetPriorityGrouping(),
					63, /* Level 0 is the highest level */
					0));
	NVIC_EnableIRQ(CCU41_1_IRQn);
}

void PinPulse_Trigger(void){
	TIMER_Clear(slice);
	XMC_GPIO_SetOutputHigh(CE_PORT, CE_PIN);
	TIMER_Start(slice);
}

void PinPulseHandler(void){
	TIMER_ClearEvent(slice);
	XMC_GPIO_SetOutputLow(CE_PORT, CE_PIN);
	TIMER_Stop(slice);
}
