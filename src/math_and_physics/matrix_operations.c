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

_STATIC_ void _Line_a_minus_line_b_times_x(uint8_t a, uint8_t b, _FLOAT_ x, Matrix_t *M1);
_STATIC_ void _Line_a_minus_line_b_times_x_on_two_matrices(uint8_t a, uint8_t b, _FLOAT_ x, Matrix_t *M1, Matrix_t *M2);
_STATIC_ _FLOAT_ secure_division(_FLOAT_ divident, _FLOAT_ divisor);
_STATIC_ void _Mat_exchange_lines_so_there_are_no_zeroes_on_the_diagonal(
		Matrix_t *M, Matrix_t *Inverse);
_STATIC_ void _Mat_scale_elements_linewise(Matrix_t* Inverse, Matrix_t* Diagonal_matrix);
_STATIC_ void _Exchange_line_a_with_line_b(uint8_t a, uint8_t b, Matrix_t *M);
_STATIC_ void Row_vector_from_(Matrix_t *M, uint8_t row_index, Vector_t *result_vect);
_STATIC_ void Column_vector_from_(Matrix_t *M, uint8_t col_index, Vector_t *result_vect);


void Mat_write(Matrix_t *M, uint8_t i, uint8_t j, _FLOAT_ value){
		M->M[i-1][j-1] = value;
}

_FLOAT_ Mat_read(Matrix_t *M, uint8_t i, uint8_t j){
		return M->M[i-1][j-1];
}

void Mat_set_all_values_to(Matrix_t *M, _FLOAT_ value){
	uint8_t i;
	uint8_t j;
	for(i = 1; i <= 3; i++){
		for(j = 1; j <= 3; j++){
			Mat_write(M, i, j, value);
		}
	}
}

void Mat_set_diag_to(Matrix_t *M, _FLOAT_ value){
	uint8_t i;
	
	for(i = 1; i <= 3; i++){
		Mat_write(M, i, i, value);
	}
}

void Mat_times_const(Matrix_t *M, _FLOAT_ constant){
	uint8_t i;
	uint8_t j;
	for(i = 1; i <= 3; i++){
		for(j = 1; j <= 3; j++){
			Mat_write(M, i, j, Mat_read(M,i,j)*constant);
		}
	}
}

void Mat_add_to(Matrix_t *M, uint8_t i, uint8_t j, _FLOAT_ value){
	_FLOAT_ result = Mat_read(M,i,j) + value;
	Mat_write(M, i , j, result);
}

void Mat_times_vect(Matrix_t *M, Vector_t *vect, Vector_t *resultvect){
	_FLOAT_ v1 = Vect_read(vect, 1);
	_FLOAT_ v2 = Vect_read(vect, 2);
	_FLOAT_ v3 = Vect_read(vect, 3);
	
	Vect_write(resultvect, 1, Mat_read(M, 1,1)*v1 + Mat_read(M,1,2)*v2 + Mat_read(M,1,3) * v3);
	Vect_write(resultvect, 2, Mat_read(M, 2,1)*v1 + Mat_read(M,2,2)*v2 + Mat_read(M,2,3) * v3);
	Vect_write(resultvect, 3, Mat_read(M, 3,1)*v1 + Mat_read(M,3,2)*v2 + Mat_read(M,3,3) * v3);
}

void Mat_transpose(Matrix_t *M, Matrix_t *result_M){
	uint8_t i;
	uint8_t j;
	POINTER_TO_CONTAINER(Matrix_t, helper);
	for(i = 1; i <= 3; i++){
		for(j = 1; j <= 3; j++){
			Mat_write(helper, j, i, Mat_read(M,i,j));
		}
	}
	Mat_copy(helper, result_M);
}
/**
 * This Function assumes that the determinant of the matrix is not zero
 */
void Mat_inverse(Matrix_t *M, Matrix_t *Inverse){
	POINTER_TO_CONTAINER(Matrix_t, Temporary);
	
	Mat_set_all_values_to(Inverse, 0.0);
	Mat_set_diag_to(Inverse, 1.0);
	
	Mat_copy(M, Temporary);
	
	_Mat_exchange_lines_so_there_are_no_zeroes_on_the_diagonal(Temporary,Inverse);
	/* I have written the 6 Matrix operations by hand,
	   because three_by_three_t is of fixed size
	   and through this i am reducing the amount of
	   'for' loops. */
	_Line_a_minus_line_b_times_x_on_two_matrices(
						     2,
						     1,
						     secure_division(Mat_read(Temporary,2,1),Mat_read(Temporary,1,1)),
						     Temporary,
						     Inverse);
	_Line_a_minus_line_b_times_x_on_two_matrices(
						     3,
						     1,
						     secure_division(Mat_read(Temporary,3,1),Mat_read(Temporary,1,1)),
						     Temporary,
						     Inverse);
	_Line_a_minus_line_b_times_x_on_two_matrices(
						     3,
						     2,
						     secure_division(Mat_read(Temporary,3,2),Mat_read(Temporary,2,2)),
						     Temporary,
						     Inverse);
	_Line_a_minus_line_b_times_x_on_two_matrices(
						     2,
						     3,
						     secure_division(Mat_read(Temporary,2,3),Mat_read(Temporary,3,3)),
						     Temporary,
						     Inverse);
	_Line_a_minus_line_b_times_x_on_two_matrices(
						     1,
						     3,
						     secure_division(Mat_read(Temporary,1,3),Mat_read(Temporary,3,3)),
						     Temporary,
						     Inverse);
	_Line_a_minus_line_b_times_x_on_two_matrices(
						     1,
						     2,
						     secure_division(Mat_read(Temporary,1,2),Mat_read(Temporary,2,2)),
						     Temporary,
						     Inverse);
	// Now Temporary contains only diagonal values. This means that it's
	// only necessairy to divide the inverse matrix lines by the corresponding 
	// values on the diagonal 
	_Mat_scale_elements_linewise(Inverse, Temporary);
}

_STATIC_ void Row_vector_from_(Matrix_t *M, uint8_t row_index, Vector_t *result_vect){
	uint8_t i;
	for(i=1; i<=3; i++){
		Vect_write(result_vect, i, Mat_read(M,row_index,i));
	}
	
}

_STATIC_ void Column_vector_from_(Matrix_t *M, uint8_t col_index, Vector_t *result_vect){
	uint8_t i;
	for(i=1; i<=3; i++){
		Vect_write(result_vect, i, Mat_read(M,i,col_index));
	}
}

void Mat_times_mat(Matrix_t *M1, Matrix_t *M2, Matrix_t *Result_M){
	uint8_t i;
	uint8_t j;
	POINTER_TO_CONTAINER(Matrix_t, helpermat);
	POINTER_TO_CONTAINER(Vector_t, row_vect);
	POINTER_TO_CONTAINER(Vector_t, col_vect);
	
	for(i=1;i<=3;i++){
		for(j=1;j<=3;j++){
			Row_vector_from_(M1, i, row_vect);
			Column_vector_from_(M2, j, col_vect);
			Mat_write(helpermat, i, j, Vect_dot(row_vect, col_vect));
		}
	}
	
	Mat_copy(helpermat, Result_M);
}

void Mat_multiply_line_a_by_x(Matrix_t *M, uint8_t a, _FLOAT_ x){
	uint8_t j;
	for(j=1;j<=3;j++){
		Mat_write(M,a,j,x * Mat_read(M,a,j));
	}
}

void Mat_copy(Matrix_t *M_from, Matrix_t *M_to){
	uint8_t i;
	uint8_t j;
	
	for(i=1;i<=3;i++){
		for(j=1;j<=3;j++){
			Mat_write(M_to,i,j,Mat_read(M_from,i,j));
		}
	}
}

_STATIC_ void _Line_a_minus_line_b_times_x_on_two_matrices(uint8_t a, uint8_t b, _FLOAT_ x, Matrix_t *M1, Matrix_t *M2){
	_Line_a_minus_line_b_times_x(a,b,x,M1);
	_Line_a_minus_line_b_times_x(a,b,x,M2);
}

_STATIC_ void _Line_a_minus_line_b_times_x(uint8_t a, uint8_t b, _FLOAT_ x, Matrix_t *M){
	uint8_t j;
	_FLOAT_ new_value;
	for(j=1;j<=3;j++){
		new_value = Mat_read(M,a,j) - x * Mat_read(M,b,j);
		Mat_write(M,a,j,new_value);
	}
}

_STATIC_ void _Mat_exchange_lines_so_there_are_no_zeroes_on_the_diagonal(
									 Matrix_t *M, Matrix_t *Inverse){
	uint8_t i;
	uint8_t k;
	uint8_t line_index_with_nonzero_value = 0;
	
	for(i=1; i<=3; i++){
		if(Mat_read(M,i,i) == 0){
			//Find the line index with a nonzero value
			for(k=1; k<=3; k++){
				if(Mat_read(M,k,i) != 0.0){
					line_index_with_nonzero_value = k;
				}
			}
			_Exchange_line_a_with_line_b(i,line_index_with_nonzero_value, M);
			_Exchange_line_a_with_line_b(i,line_index_with_nonzero_value, Inverse);
		}
	}
}

_STATIC_ void _Exchange_line_a_with_line_b(uint8_t a, uint8_t b, Matrix_t *M){
	uint8_t j;
	
	_FLOAT_ a1 = Mat_read(M,a,1);
	_FLOAT_ a2 = Mat_read(M,a,2);
	_FLOAT_ a3 = Mat_read(M,a,3);
	
	for(j=1;j<=3;j++){
		Mat_write(M, a, j, Mat_read(M,b,j));
	}
	
	Mat_write(M, b , 1, a1);
	Mat_write(M, b , 2, a2);
	Mat_write(M, b , 3, a3);
}


_STATIC_ void _Mat_scale_elements_linewise(Matrix_t* Inverse,
					   Matrix_t* Diagonal_matrix){
	// Now Temporary contains only diagonal values. This means that it's
	// only necessairy to divide the inverse matrix lines by the corresponding
	// values on the diagonal
	Mat_multiply_line_a_by_x(Inverse, 1,
				 (1.0 / Mat_read(Diagonal_matrix, 1, 1)));
	Mat_multiply_line_a_by_x(Inverse, 2,
				 (1.0 / Mat_read(Diagonal_matrix, 2, 2)));
	Mat_multiply_line_a_by_x(Inverse, 3,
				 (1.0 / Mat_read(Diagonal_matrix, 3, 3)));
}

_STATIC_ _FLOAT_ secure_division(_FLOAT_ divident, _FLOAT_ divisor){
	if(divisor){
		return divident/divisor;
	} else {
		return 0;
	}
}
