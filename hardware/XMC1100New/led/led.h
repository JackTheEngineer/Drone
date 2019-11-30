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
void LED_on(uint8_t lednum);
void LED_off(uint8_t lednum);
void LED_toggle(uint8_t lednum);

#endif /* SRC_HARDWARE_LED_LED_H_ */
