/*
 * drone_control.h
 *
 *  Created on: Oct 4, 2016
 *      Author: chocolate
 */

#ifndef TEST_DRONE_SIMULATION_DRONE_CONTROL_H_
#define TEST_DRONE_SIMULATION_DRONE_CONTROL_H_

#include "base.h"
#include "physical_definitions.h"
#include "dronedata_def.h"

void Drone_set_position(double x, double y, double z, Physical_Drone_t* drone);
void Drone_set_speed(double x, double y, double z, Physical_Drone_t *dronedata);
void Drone_set_angular_position(double pitch, double roll, double yaw, Physical_Drone_t *dronedata);


#endif /* TEST_DRONE_SIMULATION_DRONE_CONTROL_H_ */
