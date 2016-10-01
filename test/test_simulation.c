/*
 * test_simulation.c
 *
 *  Created on: Jul 30, 2016
 *      Author: chocolate
 */

#include "drone_simulation/simulation.h"
#include "test_helper.h"
#include "motors.h"
#include "drone_physics.h"
#include "timestep_definition.h"
#include "fake_motors.h"


_STATIC_ void Simulate_and_write_to_file(FILE *file);
_STATIC_ void Run_one_simulation_Step(FILE *file, Motorcontrolvalues_t *motorspeeds, double time);
_STATIC_ void Write_dronedata_to_file_in_a_line(FILE *file, double time);
_STATIC_ void Append_Vector_to_file(FILE *file, Vector_t *vector);

TEST_GROUP(simulation);

TEST_SETUP(simulation){
}

TEST_TEAR_DOWN(simulation){
}

TEST(simulation, generate_file_with_timed_data){
	FILE *file;
	file = fopen("simulation_graphics/draw_data.txt", "w");
	Simulate_and_write_to_file(file);
	fclose(file);
}

_STATIC_ void Simulate_and_write_to_file(FILE *file){
	uint32_t time_counter;
	POINTER_TO_CONTAINER(Motorcontrolvalues_t, motorspeeds);
	double time;
	
	Motors_Init();
	motorspeeds->motorspeeds[0] = 1000;
	motorspeeds->motorspeeds[1] = 1000;
	motorspeeds->motorspeeds[2] = 1000;
	motorspeeds->motorspeeds[3] = 1000;
	
	for(time_counter=0; time_counter < ITERATIONS; time_counter++){
		time = time_counter * TIMESTEP;
		Run_one_simulation_Step(file, motorspeeds, time);
	}
}

_STATIC_ void Run_one_simulation_Step(FILE *file, Motorcontrolvalues_t *motorspeeds, double time){
	Write_dronedata_to_file_in_a_line(file, time);
	Motors_Set_Speed(motorspeeds);
}

_STATIC_ void Write_dronedata_to_file_in_a_line(FILE *file, double time){
	Physical_Drone_t *drone;
	drone = fake_Motors_get_drone_pointer();
	fprintf(file, "%.5f", time);
	Append_Vector_to_file(file, &(drone->position));
	Append_Vector_to_file(file, &(drone->speed));
	Append_Vector_to_file(file, &(drone->angular_position));
	Append_Vector_to_file(file, &(drone->angular_speed));
	fprintf(file,"\t\n");
}

_STATIC_ void Append_Vector_to_file(FILE *file, Vector_t *vector){
	uint8_t i;
	for(i=1;i<=3;i++){
		fprintf(file, "\t%.5f", Vect_read(vector, i));
	}
}
