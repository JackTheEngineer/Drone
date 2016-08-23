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

#define SPEED_TO_MOMENT 0.04
extern  Masspoint_t drone_masspoints[NUMBER_OF_MASSPOINTS];

_STATIC_ void Calculate_Sum_of_thrust(Vector_t *sum_of_forces, Motor_t motors[NMBR_OF_MOTORS]);
_STATIC_ void Calculate_Sum_of_moments(Vector_t *sum_of_moments, Motor_t motors[NMBR_OF_MOTORS]);

void Drone_set_drone_data_zero(Physical_Drone_t *drone){
	Vect_set_all_values_to(&(drone->angular_position), 0.0);
	Vect_set_all_values_to(&(drone->angular_speed), 0.0);
	Vect_set_all_values_to(&(drone->position), 0.0);
	Vect_set_all_values_to(&(drone->speed), 0.0);
}
/* Differential function that calculates the Position,
 *  speed, angular position and angular speed
 *
 * timestep in seconds
 */
void Drone_calculate_next_values(Physical_Drone_t *drone, double timestep){
    POINTER_TO_CONTAINER(three_by_three_t, J_Inverse);
    POINTER_TO_CONTAINER(three_by_three_t, J);
    POINTER_TO_CONTAINER(Vector_t, sum_of_moments);
    POINTER_TO_CONTAINER(Vector_t, sum_of_forces);
    POINTER_TO_CONTAINER(Vector_t, angular_acceleration);
    POINTER_TO_CONTAINER(Vector_t, acceleration);
    double dronemass;
    Vect_set_all_values_to(sum_of_forces, 0.0);
    Vect_set_all_values_to(sum_of_moments, 0.0);

    Calculate_Sum_of_thrust(sum_of_forces, drone->motors);
    Calculate_Sum_of_moments(sum_of_forces, drone->motors);

    physics_calculate_moment_of_inertia(drone_masspoints, 8, J);
    Mat_inverse(J, J_Inverse);
    Mat_times_vect(J_Inverse, sum_of_moments, angular_acceleration);
    
    dronemass = physics_calculate_drone_mass(drone_masspoints, NUMBER_OF_MASSPOINTS);
    Vect_multiply(sum_of_forces, 1/dronemass, acceleration);
}

_STATIC_ void Calculate_Sum_of_thrust(Vector_t *sum_of_forces, Motor_t motors[NMBR_OF_MOTORS]){
    uint8_t i;
    for(i=0; i<NMBR_OF_MOTORS; i++){
        Vect_add_to(sum_of_forces, &motors[i].thrust);
    }
}

_STATIC_ void Calculate_Sum_of_moments(Vector_t *sum_of_moments, Motor_t motors[NMBR_OF_MOTORS]){
    uint8_t i;
    POINTER_TO_CONTAINER(Vector_t, helpervector);
    for(i=0;i<NMBR_OF_MOTORS;i++){
        Vect_cross_multiply(&motors[i].thrust,&motors[i].position, helpervector);
        Vect_add_to(sum_of_moments,helpervector);
        
        Vect_set_all_values_to(helpervector, 0.0);
        Vect_write(helpervector, 3, Vect_dot(&motors[i].speed,&motors[i].speed));
        Vect_multiply(helpervector, SPEED_TO_MOMENT, helpervector);
        Vect_add_to(sum_of_moments, helpervector);
    }
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
