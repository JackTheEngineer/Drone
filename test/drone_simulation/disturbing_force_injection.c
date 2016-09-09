#include "disturbing_force_injection.h"


POINTER_TO_CONTAINER(Vector_t, temp_force);
POINTER_TO_CONTAINER(Vector_t, temp_moment);

void Inject_Force_into_simulation_in_earth_frame(Vector_t *force){
	Vect_copy_from_to(force, temp_force);
}
void Get_error_force_in_earth_frame(Vector_t *force){
	Vect_copy_from_to(temp_force, force);	
}


void Inject_Moment_into_simulation_in_earth_frame(Vector_t *moment){
	Vect_copy_from_to(moment, temp_moment);
}
void Get_error_moment_in_earth_frame(Vector_t *moment){
	Vect_copy_from_to(temp_moment, moment);	
}
