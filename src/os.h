/*
 * os.h
 *
 *  Created on: Jul 23, 2017
 *      Author: chocolate
 */

#ifndef SRC_OS_H_
#define SRC_OS_H_

#include "buttons.h"
#include "motion_sensor.h"
#include "states.h"

typedef struct _os_{
	Button_t *button_1;
	Button_t *button_2;
	State_t *current_state;

	Sensordata_t *motion_sensor;
}OS_t;


#endif /* SRC_OS_H_ */
