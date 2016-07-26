/*
 * physical_helpers.c
 *
 *  Created on: May 13, 2016
 *      Author: jakov
 */

#include "matrix_operations.h"
#include "vector_operations.h"

/** This is a matrix operation library for 3 x 3 matrices. 
 *  when there is a parameter 'M', or 'M1' it's the the matrix
 *  on which the work is performed.
 */

_STATIC_ void Line_a_minus_line_b_times_x(uint8_t a, uint8_t b, double x, three_by_three_t *M1);
_STATIC_ void Line_a_minus_line_b_times_x_on_two_matrices(uint8_t a, uint8_t b, double x, three_by_three_t *M1, three_by_three_t *M2);
_STATIC_ double secure_division(double divident, double divisor);


void Mat_write(three_by_three_t *M, uint8_t i, uint8_t j, double value){
    if((i <= 3) && (j <= 3) && (i>0) && (j>0)){
        M->M[i-1][j-1] = value;
    }
}

double Mat_read(three_by_three_t *M, uint8_t i, uint8_t j){
    if((i <= 3) && (j <= 3) && (i>0) && (j>0)){
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

void Mat_times_vect(three_by_three_t *M, Vector_t *vect, Vector_t *resultvect){
    double v1 = Vect_read(vect, 1);
    double v2 = Vect_read(vect, 2);
    double v3 = Vect_read(vect, 3);
    
    Vect_write(resultvect, 1, Mat_read(M, 1,1)*v1 + Mat_read(M,1,2)*v2 + Mat_read(M,1,3) * v3);
    Vect_write(resultvect, 2, Mat_read(M, 2,1)*v1 + Mat_read(M,2,2)*v2 + Mat_read(M,2,3) * v3);
    Vect_write(resultvect, 3, Mat_read(M, 3,1)*v1 + Mat_read(M,3,2)*v2 + Mat_read(M,3,3) * v3);
}
/**
 * This Function assumes that the determinant of the matrix is not zero
 */
void Mat_inverse(three_by_three_t *M, three_by_three_t *Inverse){
    three_by_three_t M_temporary_container;
    three_by_three_t *Temporary = &M_temporary_container;
    
    Mat_set_all_values_to(Inverse, 0.0);
    Mat_set_diag_to(Inverse, 1.0);
    
    Mat_copy(M, Temporary);
    /* I have written the 6 Matrix operations by hand,
       because three_by_three_t is of fixed size
       and through this i am reducing the amount of
       'for' loops. */
    Line_a_minus_line_b_times_x_on_two_matrices(
        2,
        1,
        secure_division(Mat_read(Temporary,2,1),Mat_read(Temporary,1,1)),
        Temporary,
        Inverse);
    Line_a_minus_line_b_times_x_on_two_matrices(
        3,
        1,
        secure_division(Mat_read(Temporary,3,1),Mat_read(Temporary,1,1)),
        Temporary,
        Inverse);
    Line_a_minus_line_b_times_x_on_two_matrices(
        3,
        2,
        secure_division(Mat_read(Temporary,3,2),Mat_read(Temporary,2,2)),
        Temporary,
        Inverse);
    Line_a_minus_line_b_times_x_on_two_matrices(
        2,
        3,
        secure_division(Mat_read(Temporary,2,3),Mat_read(Temporary,3,3)),
        Temporary,
        Inverse);
    Line_a_minus_line_b_times_x_on_two_matrices(
        1,
        3,
        secure_division(Mat_read(Temporary,1,3),Mat_read(Temporary,3,3)),
        Temporary,
        Inverse);
    Line_a_minus_line_b_times_x_on_two_matrices(
        1,
        2,
        secure_division(Mat_read(Temporary,1,2),Mat_read(Temporary,2,2)),
        Temporary,
        Inverse);
    // Now Temporary contains only diagonal values. This means that it's
    // only necessairy to divide the inverse matrix lines by the corresponding 
    // values on the diagonal 
    Mat_multiply_line_a_by_x(Inverse,1,(1.0/Mat_read(Temporary,1,1)));
    Mat_multiply_line_a_by_x(Inverse,2,(1.0/Mat_read(Temporary,2,2)));
    Mat_multiply_line_a_by_x(Inverse,3,(1.0/Mat_read(Temporary,3,3)));
}

void Mat_times_mat(three_by_three_t *M1, three_by_three_t *M2, three_by_three_t *Result_M){
    
}

void Mat_multiply_line_a_by_x(three_by_three_t *M, uint8_t a, double x){
    uint8_t j;
    for(j=1;j<=3;j++){
        Mat_write(M,a,j,x * Mat_read(M,a,j));
    }
}

void Mat_copy(three_by_three_t *M_from, three_by_three_t *M_to){
    uint8_t i;
    uint8_t j;
    
    for(i=1;i<=3;i++){
        for(j=1;j<=3;j++){
            Mat_write(M_to,i,j,Mat_read(M_from,i,j));
        }
    }
}

_STATIC_ void Line_a_minus_line_b_times_x_on_two_matrices(uint8_t a, uint8_t b, double x, three_by_three_t *M1, three_by_three_t *M2){
    Line_a_minus_line_b_times_x(a,b,x,M1);
    Line_a_minus_line_b_times_x(a,b,x,M2);
}

_STATIC_ void Line_a_minus_line_b_times_x(uint8_t a, uint8_t b, double x, three_by_three_t *M){
    uint8_t j;
    double new_value;
    for(j=1;j<=3;j++){
        new_value = Mat_read(M,a,j) - x * Mat_read(M,b,j);
        Mat_write(M,a,j,new_value);
    }
}

_STATIC_ double secure_division(double divident, double divisor){
    if(divisor){
        return divident/divisor;
    } else {
        printf("divisor was zero !! \n");
        return 0;
    }
}
