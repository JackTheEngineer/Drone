/*
 * simulation_connection.h
 *
 *  Created on: Jul 28, 2016
 *      Author: chocolate
 */

#ifndef TEST_DRONE_SIMULATION_SIMULATION_CONNECTION_H_
#define TEST_DRONE_SIMULATION_SIMULATION_CONNECTION_H_

#include "base.h"
#include "fake_motion_sensor.h"
#include "fake_motors.h"

void Simulation_recieve_motorcurrents(Motorcurrents_t *motorcurrents);
void Simulation_write_sensordata(Sensordata_t *sensordata);

#endif /* TEST_DRONE_SIMULATION_SIMULATION_CONNECTION_H_ */
