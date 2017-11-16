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

XMC_GPIO_CONFIG_t const gpio_config = {
		.mode = XMC_GPIO_MODE_OUTPUT_PUSH_PULL,
		.output_level = XMC_GPIO_OUTPUT_LEVEL_LOW,
};

void PinPulse_Init(void){
	XMC_GPIO_Init(CE_PORT, CE_PIN, &gpio_config);

	TIMER_Init();
}

void PinPulse_Trigger(void){
	TIMER_Clear();
	XMC_GPIO_SetOutputHigh(CE_PORT, CE_PIN);
	TIMER_Start();
}

void CCU41_1_IRQHandler(void){
	TIMER_ClearEvent();
	XMC_GPIO_SetOutputLow(CE_PORT, CE_PIN);
	TIMER_Stop();
}
