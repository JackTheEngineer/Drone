#ifndef DRONE_DATA_DEF
#define DRONE_DATA_DEF

#include "motordata_def.h"
#include "physical_definitions.h"
#include "drone_constants.h"

/* Used as Absolute position from a point Zero */
typedef struct Drone_Data{
	Vector_t position;
	Vector_t angular_position;
	Vector_t speed;
	Vector_t angular_speed;
	Vector_t acceleration;
	Motor_t motors[NMBR_OF_MOTORS];
	Matrix_t J_Inverse;
}Physical_Drone_t;

#endif /* DRONE_DATA_DEF */
