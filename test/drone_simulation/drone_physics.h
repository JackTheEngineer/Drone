/*
 * drone_simulation.h
 *
 *  Created on: Apr 23, 2016
 *      Author: jakov
 */

#ifndef TEST_SUPPORT_DRONE_SIMULATION_H_
#define TEST_SUPPORT_DRONE_SIMULATION_H_

#include "base.h"
#include "physical_definitions.h"
#include "drone_constants.h"
#include "dronedata_def.h"

Physical_Drone_t* Drone_get_dronedata(void);

void Drone_calculate_next_values(Physical_Drone_t *drone, double timestep);
void Drone_calculate_inverse_mass_matrix(Matrix_t *J_Inverse);
void Drone_set_drone_data_zero(Physical_Drone_t *dronedata);
void Generate_Rotation_Matrix(Matrix_t *rotation_matrix, Vector_t *angles);
void Rotate_from_drone_frame_to_earth_frame(Vector_t *vect_to_rotate, Vector_t *angles_for_rotation);
void Rotate_from_earth_frame_to_drone_frame(Vector_t *vect_to_rotate, Vector_t *angles_for_rotation);

#endif /* TEST_SUPPORT_DRONE_SIMULATION_H_ */
