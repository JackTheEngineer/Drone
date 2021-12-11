/*
 * os.h
 *
 *  Created on: Jul 23, 2017
 *      Author: chocolate
 */

#ifndef SRC_OS_H_
#define SRC_OS_H_

#include "application/motion_sensor/motion_sensor.h"
#include "buttons.h"
#include "quaternions.h"
#include "states.h"

typedef struct _os_{
	Button_t *button_1;
	Button_t *button_2;
	State_t current_state;
	Quaternion_t *base_quat;
	Quaternion_t *position_quat;
	Sensordata_t *motion_sensor;
}OS_t;


#endif /* SRC_OS_H_ */
