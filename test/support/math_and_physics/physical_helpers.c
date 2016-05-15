/*
 * physical_helpers.c
 *
 *  Created on: May 13, 2016
 *      Author: jakov
 */

#include "matrix_operations.h"
#include "physical_helpers.h"
#include "math_helpers.h"


void physics_calculate_moment_of_inertia(Masspoint_t *masspoints, uint32_t  number_of_masspoints, three_by_three_t *J){
	uint8_t i;
    uint8_t j;
    uint8_t k;
    double r_squared;
    double pointmass;
    Vector_t* vector;

    for(k = 0; k < number_of_masspoints; k++){
    	pointmass = masspoints[k].m;
        vector = &(masspoints[k].v);
        r_squared = SQR(vector->x) + SQR(vector->y) + SQR(vector->z);

    	for(i = 1; i <= 3; i++){
            for(j = 1; j <= 3; j++){
                Mat_add_to(J, i, j, pointmass*(r_squared*(double)kron_delta(i,j) - vect_val_of_ind(vector, j)*vect_val_of_ind(vector, i)));
            }   
        }
    }
}

