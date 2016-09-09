/*
 * drone_simulation.c
 *
 *  Created on: Apr 23, 2016
 *      Author: jakov
 */
#include "drone_physics.h"
#include "vector_operations.h"
#include "propeller.h"
#include "physical_helpers.h"
#include "matrix_operations.h"
#include "disturbing_force_injection.h"

#define SPEED_TO_MOMENT 0.04
#define G_ACCELERATION -9.81
extern  Masspoint_t drone_masspoints[NUMBER_OF_MASSPOINTS];

_STATIC_ void Calculate_Sum_of_forces(Vector_t *sum_of_forces, Physical_Drone_t *drone);
_STATIC_ void Calculate_Sum_of_moments(Vector_t *sum_of_moments, Physical_Drone_t *drone);
_STATIC_ void Integrate_with_given_accelerations(Vector_t* acceleration, Vector_t* speed, Vector_t* position, double timestep);
_STATIC_ void Calculate_Moment_from_rotorspeed(Motor_t *motor, Vector_t *resulting_moment);

void Drone_set_drone_data_zero(Physical_Drone_t *drone){
	Vect_set_all_values_to(&(drone->angular_position), 0.0);
	Vect_set_all_values_to(&(drone->angular_speed), 0.0);
	Vect_set_all_values_to(&(drone->position), 0.0);
	Vect_set_all_values_to(&(drone->speed), 0.0);
}

void Generate_Rotation_Matrix(Matrix_t *rotation_matrix, Vector_t *angles){
	/*
	  Generate an R(zyx) rotation Matrix
	*/
	POINTER_TO_CONTAINER(Matrix_t, R_x);
	Mat_set_all_values_to(R_x, 0.0);
	POINTER_TO_CONTAINER(Matrix_t, R_y);
	Mat_set_all_values_to(R_y, 0.0);
	POINTER_TO_CONTAINER(Matrix_t, R_z);
	Mat_set_all_values_to(R_z, 0.0);
	POINTER_TO_CONTAINER(Matrix_t, helpermat);
	Mat_set_all_values_to(helpermat, 0.0);
	
	Mat_write(R_x, 1,1, 1.0);
	Mat_write(R_x, 2,2, cos(Vect_read(angles, 1)));
	Mat_write(R_x, 2,3, -sin(Vect_read(angles,1)));
	Mat_write(R_x, 3,3, cos(Vect_read(angles,1)));
	Mat_write(R_x, 3,2, sin(Vect_read(angles,1)));
	
	Mat_write(R_y, 2,2, 1.0);
	Mat_write(R_y, 1,1, cos(Vect_read(angles, 2)));
	Mat_write(R_y, 1,3, -sin(Vect_read(angles,2)));
	Mat_write(R_y, 3,3, cos(Vect_read(angles,2)));
	Mat_write(R_y, 3,1, sin(Vect_read(angles,2)));
	
	Mat_write(R_z, 3,3, 1.0);
	Mat_write(R_z, 1,1, cos(Vect_read(angles,3)));
	Mat_write(R_z, 1,2, -sin(Vect_read(angles,3)));
	Mat_write(R_z, 2,2, cos(Vect_read(angles,3)));
	Mat_write(R_z, 2,1, sin(Vect_read(angles,3)));
	
	/* Last parameter is the matrix to which the result is saved */
	Mat_times_mat(R_z,R_y,R_y); 
	Mat_times_mat(R_y,R_x,rotation_matrix);
}

void Rotate_from_earth_frame_to_drone_frame(Vector_t *vect_to_rotate, Vector_t *angles_for_rotation){
	POINTER_TO_CONTAINER(Matrix_t, rotation_matrix);

	Generate_Rotation_Matrix(rotation_matrix, angles_for_rotation);
	Mat_transpose(rotation_matrix, rotation_matrix);
	Mat_times_vect(rotation_matrix, vect_to_rotate, vect_to_rotate);
}

void Rotate_from_drone_frame_to_earth_frame(Vector_t *vect_to_rotate, Vector_t *angles_for_rotation){
	POINTER_TO_CONTAINER(Matrix_t, rotation_matrix);
	
	Generate_Rotation_Matrix(rotation_matrix, angles_for_rotation);
	Mat_times_vect(rotation_matrix, vect_to_rotate, vect_to_rotate);
}

/* Differential function that calculates the Position,
 *  speed, angular position and angular speed
 *
 * timestep in seconds
 */
void Drone_calculate_next_values(Physical_Drone_t *drone, double timestep){
	POINTER_TO_CONTAINER(Matrix_t, J_Inverse);
	POINTER_TO_CONTAINER(Matrix_t, J);
	POINTER_TO_CONTAINER(Vector_t, sum_of_moments);
	POINTER_TO_CONTAINER(Vector_t, sum_of_forces);
	POINTER_TO_CONTAINER(Vector_t, angular_acceleration);
	POINTER_TO_CONTAINER(Vector_t, acceleration);
	POINTER_TO_CONTAINER(Vector_t, g_acceleration);
	double dronemass;
	
	Vect_set_all_values_to(g_acceleration, 0.0);
	Vect_set_all_values_to(sum_of_forces, 0.0);
	Vect_set_all_values_to(sum_of_moments, 0.0);
	Vect_write(g_acceleration, 3, G_ACCELERATION);
	
	/* Forces are being calculated in the Drone reference frame */
	Calculate_Sum_of_forces(sum_of_forces, drone);
	Calculate_Sum_of_moments(sum_of_moments, drone);
	
	physics_calculate_moment_of_inertia(drone_masspoints, 8, J);
	Mat_inverse(J, J_Inverse);
	Mat_times_vect(J_Inverse, sum_of_moments, angular_acceleration);
	Rotate_from_drone_frame_to_earth_frame(angular_acceleration, &(drone->angular_position));
    
	dronemass = physics_calculate_drone_mass(drone_masspoints, NUMBER_OF_MASSPOINTS);
	Vect_times_const(sum_of_forces, 1/dronemass, acceleration);
	
	Rotate_from_drone_frame_to_earth_frame(acceleration, &(drone->angular_position));
	Vect_add_to(acceleration, g_acceleration);
	
	Integrate_with_given_accelerations(acceleration, &(drone->speed), &(drone->position), timestep);
	Integrate_with_given_accelerations(angular_acceleration, &(drone->angular_speed), &(drone->angular_position), timestep);
}

_STATIC_ void Integrate_with_given_accelerations(Vector_t* acceleration, Vector_t* speed, Vector_t* position, double timestep){
	POINTER_TO_CONTAINER(Vector_t, helpervector);
	
	Vect_times_const(acceleration, timestep, helpervector);
	Vect_add_to(speed, helpervector);
	Vect_times_const(speed, timestep, helpervector);
	Vect_add_to(position, helpervector);
}

_STATIC_ void Calculate_Sum_of_forces(Vector_t *sum_of_forces, Physical_Drone_t *drone){
	POINTER_TO_CONTAINER(Vector_t, disturbing_force);
	
	uint8_t i;
	for(i=0; i<NMBR_OF_MOTORS; i++){
		Vect_add_to(sum_of_forces, &drone->motors[i].thrust);
	}
	Get_error_force_in_earth_frame(disturbing_force);
	Rotate_from_earth_frame_to_drone_frame(disturbing_force, &(drone->angular_position));
	Vect_add_to(sum_of_forces, disturbing_force);
}

_STATIC_ void Calculate_Sum_of_moments(Vector_t *sum_of_moments,Physical_Drone_t *drone){
	uint8_t i;
	POINTER_TO_CONTAINER(Vector_t, helpervector);
	
	for(i=0;i<NMBR_OF_MOTORS;i++){
		Vect_cross_multiply(&drone->motors[i].thrust,&drone->motors[i].position, helpervector);
		Vect_add_to(sum_of_moments,helpervector);
		
		Calculate_Moment_from_rotorspeed(&(drone->motors[i]), helpervector);
		Vect_add_to(sum_of_moments, helpervector);
	}
	Get_error_moment_in_earth_frame(helpervector);
	Rotate_from_earth_frame_to_drone_frame(helpervector, &(drone->angular_position));
	Vect_add_to(sum_of_moments, helpervector);
}

_STATIC_ void Calculate_Moment_from_rotorspeed(Motor_t *motor, Vector_t *resulting_moment){
	POINTER_TO_CONTAINER(Vector_t, unitary_vector);
	POINTER_TO_CONTAINER(Vector_t, speed);
	speed = &(motor->speed);
	Vect_uniform(speed, unitary_vector);
	Vect_times_const(unitary_vector, SPEED_TO_MOMENT*Vect_dot(speed,speed), resulting_moment);
}

void Drone_set_position(double x, double y, double z, Physical_Drone_t *drone){
	Vector_t *position = &(drone->position);
	Vect_write(position, 1, x);
	Vect_write(position, 2, y);
	Vect_write(position, 3, z);
}

void Drone_set_speed(double x, double y, double z, Physical_Drone_t *drone){
	Vector_t *speed = &(drone->speed);
	Vect_write(speed, 1, x);
	Vect_write(speed, 2, y);
	Vect_write(speed, 3, z);
}

void Drone_set_angular_position(double pitch, double roll, double yaw, Physical_Drone_t *drone){
	Vector_t *angle = &(drone->angular_position);
	Vect_write(angle, 1, pitch);
	Vect_write(angle, 2, roll);
	Vect_write(angle, 3, yaw);
}
