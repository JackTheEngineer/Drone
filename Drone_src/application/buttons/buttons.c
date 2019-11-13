/*
 * buttons.c
 *
 *  Created on: Jul 22, 2017
 *      Author: chocolate
 */

#include "buttons.h"
#include "xmc_gpio.h"

const XMC_GPIO_CONFIG_t button_config   =
{
		.mode                = XMC_GPIO_MODE_INPUT_INVERTED_TRISTATE,
		.output_level        = XMC_GPIO_OUTPUT_LEVEL_HIGH,
		.output_strength     = XMC_GPIO_OUTPUT_STRENGTH_STRONG_SHARP_EDGE,
};

void buttons_init(void){
	XMC_GPIO_Init(XMC_GPIO_PORT15, 13, &button_config);
	XMC_GPIO_Init(XMC_GPIO_PORT15, 12, &button_config);
}

uint32_t button_read(Hdw_button_t button){
	if(button == _BUTTON1){
		return XMC_GPIO_GetInput(XMC_GPIO_PORT15, 13);
	}
	if(button == _BUTTON2){
		return XMC_GPIO_GetInput(XMC_GPIO_PORT15, 12);
	}
	return 0;
}

Edge_t button_readEdge(Button_t *btn){
	Edge_t returnval = NO_EDGE;
	bool cur_state = (bool)button_read(btn->btn);

	if((btn->laststate == false) &&
			(cur_state == true)){
		returnval = RISING_EDGE;
	}
	if((btn->laststate == true) &&
			(cur_state == false)){
		returnval = FALLING_EDGE;
	}
	btn->laststate = cur_state;
	return returnval;
}
