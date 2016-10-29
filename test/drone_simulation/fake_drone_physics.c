#include "motion_sensor.h"
#include "drone_physics.h"

STATIC_POINTER_TO_CONTAINER(Physical_Drone_t, dronedata);

void Drone_calculate_inverse_mass_matrix(Matrix_t *inverse){

}

void Drone_set_drone_data_zero(Physical_Drone_t *drone){
	Vect_set_all_values_to(&(drone->angular_position), 0.0);
	Vect_set_all_values_to(&(drone->angular_speed), 0.0);
	Vect_set_all_values_to(&(drone->position), 0.0);
	Vect_set_all_values_to(&(drone->speed), 0.0);
	Vect_set_all_values_to(&(drone->acceleration), 0.0);
}

Physical_Drone_t* Drone_get_dronedata(void){
	return dronedata;
}

void Drone_calculate_next_values(Physical_Drone_t *drone, double timestep){

}
