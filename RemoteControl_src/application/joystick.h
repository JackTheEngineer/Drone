/*
 * joystick.h
 *
 *  Created on: Jun 7, 2018
 *      Author: jakov
 */

#ifndef SRC_APPLICATION_JOYSTICK_H_
#define SRC_APPLICATION_JOYSTICK_H_

#include "base.h"
#include "ADC_conversion_decoding.h"

void Joystick_Init(void);
void Joysticks_get_newest_values(uint16_t results[NUM_OF_MEASURED_CHANNELS]);
void Joystick_serialize_data(uint16_t joystick_data[4], uint8_t *targetbuf);



#endif /* SRC_APPLICATION_JOYSTICK_H_ */
