/*
 * physical_helpers.c
 *
 *  Created on: May 13, 2016
 *      Author: jakov
 */

#include "matrix_operations.h"
#include "math_helpers.h"

void Mat_write(three_by_three_t *M, uint8_t i, uint8_t j, double value){
    if((i <= 3) && (j <= 3)){
	M->M[i-1][j-1] = value;
    }
}

double Mat_read(three_by_three_t *M, uint8_t i, uint8_t j){
    if((i <= 3) && (j <= 3)){
        return M->M[i-1][j-1];
    }
    return 0.0;

}

void Mat_set_all_values_to(three_by_three_t *M, double value){
    uint8_t i;
    uint8_t j;
    for(i = 1; i <= 3; i++){
        for(j = 1; j <= 3; j++){       
            Mat_write(M, i, j, value);
        }
    }
}

void Mat_set_diag_to(three_by_three_t *M, double value){
    uint8_t i;

    for(i = 1; i <= 3; i++){
        Mat_write(M, i, i, value);
    }
}

void Mat_mult_with_const(three_by_three_t *M, double constant){
	uint8_t i;
	uint8_t j;
	for(i = 1; i <= 3; i++){
		for(j = 1; j <= 3; j++){
			Mat_write(M, i, j, Mat_read(M,i,j)*constant);
		}
	}
}


void Mat_add_to(three_by_three_t *M, uint8_t i, uint8_t j, double value){
	Mat_write(M,i,j, Mat_read(M,i,j) + value);
}
