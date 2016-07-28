/*
 * propeller.c
 *
 *  Created on: May 19, 2016
 *      Author: jakov
 */

#include "propeller.h"

#define PROP_CONSTANT 5.0

double propeller_force_of_rpm(double rpm){
    return rpm * PROP_CONSTANT;
}

double propeller_rpm_of_force(double force){
    return force * 1/PROP_CONSTANT;
}
