/*
 * propeller.h
 *
 *  Created on: May 19, 2016
 *      Author: jakov
 */

#ifndef TEST_SUPPORT_DRONE_SIMULATION_PROPELLER_H_
#define TEST_SUPPORT_DRONE_SIMULATION_PROPELLER_H_

#include "base.h"


double propeller_force_of_rpm(double rpm);
double propeller_rpm_of_force(double force);

#endif /* TEST_SUPPORT_DRONE_SIMULATION_PROPELLER_H_ */
