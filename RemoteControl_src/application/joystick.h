/*
 * joystick.h
 *
 *  Created on: Jun 7, 2018
 *      Author: jakov
 */

#ifndef SRC_APPLICATION_JOYSTICK_H_
#define SRC_APPLICATION_JOYSTICK_H_

#include "base.h"

typedef struct Joystick{
	uint16_t vertical;
	uint16_t horizontal;
}Joystick_t;


void Joystick_Init(void);
void Joysticks_get_current_values(Joystick_t *left_joystick, Joystick_t *right_joystick);
void Joystick_serialize_data(Joystick_t *l_joystick, Joystick_t *r_joystick, uint8_t *sendbytes);



#endif /* SRC_APPLICATION_JOYSTICK_H_ */
