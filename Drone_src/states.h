/*
 * states.h
 *
 *  Created on: Jun 10, 2018
 *      Author: jakov
 */

#ifndef SRC_STATES_H_
#define SRC_STATES_H_

typedef enum _states_{
	STATE_INITIALIZE,
	STATE_CALIBRATE_MOTION_SENSOR,
	STATE_CALIBRATE_QUATERNION,
	STATE_RUN,
}State_t;

#endif /* SRC_STATES_H_ */
