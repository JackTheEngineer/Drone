/*
 * physical_definitions.h
 *
 *  Created on: May 13, 2016
 *      Author: jakov
 */

#ifndef TEST_PHYSICAL_DEFINITIONS_H_
#define TEST_PHYSICAL_DEFINITIONS_H_

#include "base.h"

typedef struct Vector{
	double x;
	double y;
	double z;
}Vector_t;

typedef struct Masspoint{
	Vector_t v;
	double m;
}Masspoint_t;


#endif /* TEST_PHYSICAL_DEFINITIONS_H_ */
