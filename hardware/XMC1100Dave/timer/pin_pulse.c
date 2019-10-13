/*
 * pin_pulse.c
 *
 *  Created on: Oct 31, 2017
 *      Author: chocolate
 */


#include "pin_pulse.h"

#include "timer.h"
#include "xmc_gpio.h"
#include "xmc_scu.h"

XMC_GPIO_CONFIG_t const gpio_config = {
		.mode = XMC_GPIO_MODE_OUTPUT_PUSH_PULL,
		.output_level = XMC_GPIO_OUTPUT_LEVEL_LOW,
};

void PinPulse_Init(void){
	XMC_GPIO_Init(CE_PORT, CE_PIN, &gpio_config);

	NVIC_SetPriority(CCU40_0_IRQn, 3);
	NVIC_EnableIRQ(CCU40_0_IRQn);

	TIMER_Init_with_params((XMC_CCU4_MODULE_t*) CCU40,
			(XMC_CCU4_SLICE_t*) CCU40_CC40,
			(XMC_CCU4_SLICE_COMPARE_CONFIG_t * const)&TIMER_0_config,
			0);
}

void PinPulse_Trigger(void){
	TIMER_Clear((XMC_CCU4_SLICE_t*) CCU40_CC40);
	XMC_GPIO_SetOutputHigh(CE_PORT, CE_PIN);
	TIMER_Start((XMC_CCU4_SLICE_t*) CCU40_CC40);
}

void PinPulseHandler(void){
	TIMER_ClearEvent((XMC_CCU4_SLICE_t*) CCU40_CC40);
	XMC_GPIO_SetOutputLow(CE_PORT, CE_PIN);
	TIMER_Stop((XMC_CCU4_SLICE_t*) CCU40_CC40);
}

