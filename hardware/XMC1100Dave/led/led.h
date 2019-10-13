/*
 * led.h
 *
 *  Created on: Oct 19, 2017
 *      Author: chocolate
 */

#ifndef SRC_HARDWARE_LED_LED_H_
#define SRC_HARDWARE_LED_LED_H_

#include "base.h"
#include "xmc_gpio.h"

void LED_init();
void LED_on(void);
void LED_off(void);
void LED_toggle(void);

#endif /* SRC_HARDWARE_LED_LED_H_ */
