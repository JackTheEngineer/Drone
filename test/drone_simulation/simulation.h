/*
 * simulation_connection.h
 *
 *  Created on: Jul 28, 2016
 *      Author: chocolate
 */

#ifndef TEST_DRONE_SIMULATION_SIMULATION_H_
#define TEST_DRONE_SIMULATION_SIMULATION_H_

#include "base.h"
#include "fake_motion_sensor.h"
#include "fake_motors.h"
#include "drone_physics.h"

void Simulation_init(Physical_Drone_t *dronedata_from_external);
void Simulation_recieve(Physical_Drone_t *drone);
void Simulation_write_sensordata(Sensordata_t *sensordata);

#endif /* TEST_DRONE_SIMULATION_SIMULATION_H_ */
