
#ifndef _MATH__HELPERS__
#define _MATH__HELPERS__

#include "base.h"

typedef struct Vector{
	double x;
	double y;
	double z;
}Vector_t;

double Vect_read(Vector_t *vect, uint8_t index);
bool kron_delta(uint8_t ind_1, uint8_t ind_2);
double Vect_length(Vector_t *vect);
double Vect_dot(Vector_t *vect_1, Vector_t *vect_2);
void Vect_set_all_values_to(Vector_t *vect, double value);
void Vect_write(Vector_t *vect, uint8_t index, double value);
void Vect_add_to(Vector_t *sum_vect, Vector_t *vect);
void Vect_add(Vector_t *vect_1, Vector_t *vect_2, Vector_t *sum_vect);
void Vect_uniform(Vector_t *vector, Vector_t *uniformed_vector);
void Vect_write_three_values(Vector_t *vector, double value_1, double value_2, double value_3);
void Vect_multiply(Vector_t *vector, double constant, Vector_t *result_vector);
void Vect_copy_from_to(Vector_t *vector_from, Vector_t *vector_to);
void Vect_cross_multiply(Vector_t *vector_1, Vector_t *vector_2, Vector_t *resultvector);
void Vect_sum_up_list_of_vectors(Vector_t *vectorlist, Vector_t *sum_vector, uint8_t listlength);
void Vect_set_vectorlist_to_value(Vector_t vectorlist[], uint32_t listlength, double value);
double *Vect_pointer_to_index(Vector_t *vect, uint8_t index);

#endif
