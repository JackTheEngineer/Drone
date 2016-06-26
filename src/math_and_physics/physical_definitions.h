/*
 * physical_definitions.h
 *
 *  Created on: May 13, 2016
 *      Author: jakov
 */

#ifndef TEST_PHYSICAL_DEFINITIONS_H_
#define TEST_PHYSICAL_DEFINITIONS_H_

#include "base.h"
#include "vector_operations.h"

typedef struct Masspoint{
	Vector_t v;
	double m;
}Masspoint_t;

typedef struct ThreebyThree{
	double M[3][3];
}three_by_three_t;


#endif /* TEST_PHYSICAL_DEFINITIONS_H_ */
