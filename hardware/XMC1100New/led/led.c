/*
 * led.c
 *
 *  Created on: Oct 19, 2017
 *      Author: chocolate
 */

#include "led.h"
#include <xmc_gpio.h>

#define NUMPINS 2

typedef struct _Portpin_{
	XMC_GPIO_PORT_t *const port;
	uint8_t pin;
}Portpin_t;

const Portpin_t portpins[NUMPINS] = {
		{
				.port = XMC_GPIO_PORT1,
				.pin = 0,
		},		{
				.port = XMC_GPIO_PORT1,
				.pin = 1,
		},
};

_STATIC_ XMC_GPIO_CONFIG_t gpio_config = {
    .mode = XMC_GPIO_MODE_OUTPUT_PUSH_PULL,
    .output_level = XMC_GPIO_OUTPUT_LEVEL_LOW,
};

void LED_init(void){
	uint8_t i;

	for(i = 0; i < NUMPINS; i++)
	{
		XMC_GPIO_Init(portpins[i].port, portpins[i].pin, &gpio_config);
	}
}

void LED_on(uint8_t lednum){
	if(lednum >= NUMPINS){ return;}
	XMC_GPIO_SetOutputHigh(portpins[lednum].port, portpins[lednum].pin);
}

void LED_off(uint8_t lednum){
	if(lednum >= NUMPINS){ return;}
	XMC_GPIO_SetOutputLow(portpins[lednum].port, portpins[lednum].pin);
}

void LED_toggle(uint8_t lednum){
	if(lednum >= NUMPINS){ return;}
	XMC_GPIO_ToggleOutput(portpins[lednum].port, portpins[lednum].pin);
}

