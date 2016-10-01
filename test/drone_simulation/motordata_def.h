#ifndef MOTOR_DATA_DEF
#define MOTOR_DATA_DEF

#include "vector_operations.h"

typedef struct Motor{
	_FLOAT_ current;
	Vector_t speed;
	Vector_t thrust;
	Vector_t position;
}Motor_t;

#endif /* MOTOR_DATA_DEF */
