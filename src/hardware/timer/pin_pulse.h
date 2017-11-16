/*
 * pin_pulse.h
 *
 *  Created on: Oct 31, 2017
 *      Author: chocolate
 */

#ifndef SRC_HARDWARE_TIMER_PIN_PULSE_PIN_PULSE_H_
#define SRC_HARDWARE_TIMER_PIN_PULSE_PIN_PULSE_H_

#include "xmc_gpio.h"

#define CE_PORT XMC_GPIO_PORT0
#define CE_PIN 5

void PinPulse_Init(void);
void PinPulse_Trigger(void);

#endif /* SRC_HARDWARE_TIMER_PIN_PULSE_PIN_PULSE_H_ */
