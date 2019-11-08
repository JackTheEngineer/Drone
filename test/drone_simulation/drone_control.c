/*
 * drone_control.c
 *
 *  Created on: Oct 4, 2016
 *      Author: chocolate
 */

#include "drone_control.h"

void Drone_set_position(double x, double y, double z, Physical_Drone_t *drone){
	Vector_t *position = &(drone->position);
	Vect_write(position, 0, x);
	Vect_write(position, 1, y);
	Vect_write(position, 2, z);
}

void Drone_set_speed(double x, double y, double z, Physical_Drone_t *drone){
	Vector_t *speed = &(drone->speed);
	Vect_write(speed, 0, x);
	Vect_write(speed, 1, y);
	Vect_write(speed, 2, z);
}

void Drone_set_angular_position(double pitch, double roll, double yaw, Physical_Drone_t *drone){
	Vector_t *angle = &(drone->angular_position);
	Vect_write(angle, 0, pitch);
	Vect_write(angle, 1, roll);
	Vect_write(angle, 2, yaw);
}
