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

#define NUMBER_OF_MASSPOINTS 8

typedef struct Masspoint{
	Vector_t v;
	_FLOAT_ m;
}Masspoint_t;

typedef struct Force{
	Vector_t force;
	Vector_t point_of_application;
}Force_t;

typedef struct ThreebyThree{
	_FLOAT_ M[3][3];
}Matrix_t;


#endif /* TEST_PHYSICAL_DEFINITIONS_H_ */
