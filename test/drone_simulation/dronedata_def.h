#ifndef DRONE_DATA_DEF
#define DRONE_DATA_DEF

#include "motordata_def.h"

/* Used as Absolute position from a point Zero */
typedef struct Drone_Data{
	Vector_t position;
	Vector_t angular_position;
	Vector_t speed;
	Vector_t angular_speed;
	Motor_t motors[NMBR_OF_MOTORS];
}Physical_Drone_t;

#endif /* DRONE_DATA_DEF */