/*
 * physical_helpers.c
 *
 *  Created on: May 13, 2016
 *      Author: jakov
 */

#include "matrix_operations.h"
#include "physical_helpers.h"

#include "vector_operations.h"

void physics_calculate_moment_of_inertia(Masspoint_t *masspoints, uint32_t  number_of_masspoints, Matrix_t *J){
	uint8_t i;
    uint8_t j;
    uint8_t k;
    double x;
    double y;
    double z;
    double r_squared;
    double pointmass;
    Vector_t* vector;
    for(k = 0; k < number_of_masspoints; k++){
    	pointmass = masspoints[k].m;
        vector = &(masspoints[k].v);
	x = Vect_read(vector, 1);
	y = Vect_read(vector, 2);
	z = Vect_read(vector, 3);
	r_squared = SQR(x) + SQR(y) + SQR(z);

    	for(i = 1; i <= 3; i++){
            for(j = 1; j <= 3; j++){
                Mat_add_to(J, i, j, pointmass*(r_squared*(double)kron_delta(i,j) - Vect_read(vector, j)*Vect_read(vector, i)));
            }   
        }
    }
}

double physics_calculate_drone_mass(Masspoint_t const *masspoints, uint32_t number_of_masspoints){
    uint32_t i;
    double right_mass=0;
    for(i=0; i<number_of_masspoints; i++){
        right_mass += masspoints[i].m;
    }
    return right_mass;
}

