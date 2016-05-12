/*
 * rc_control.h
 *
 *  Created on: Apr 22, 2016
 *      Author: jakov
 */

#ifndef RC_CONTROL_H_
#define RC_CONTROL_H_

#include "base.h"

typedef struct Flight_Data{
	uint32_t throttle;
	int32_t rotation;
	int32_t x_tilt;
	int32_t y_tilt;
}Flight_Data_t;



#endif /* RC_CONTROL_H_ */
