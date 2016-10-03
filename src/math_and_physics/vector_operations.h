
#ifndef _MATH__HELPERS__
#define _MATH__HELPERS__

#include "base.h"

typedef struct Vector{
	_FLOAT_ v[3];
}Vector_t;

typedef struct Vector_i32{
	int32_t v[3];
}Vector_i32_t;


bool kron_delta(uint8_t ind_1, uint8_t ind_2);

_FLOAT_ Vect_read(Vector_t *vect, uint8_t index);
_FLOAT_ Vect_length(Vector_t *vect);
_FLOAT_ Vect_dot(Vector_t *vect_1, Vector_t *vect_2);
void Vect_set_all_values_to(Vector_t *vect, _FLOAT_ value);
void Vect_write(Vector_t *vect, uint8_t index, _FLOAT_ value);
void Vect_add_to(Vector_t *sum_vect, Vector_t *vect);
void Vect_add(Vector_t *vect_1, Vector_t *vect_2, Vector_t *sum_vect);
void Vect_uniform(Vector_t *vector, Vector_t *uniformed_vector);
void Vect_write_three_values(Vector_t *vector, _FLOAT_ value_1, _FLOAT_ value_2, _FLOAT_ value_3);
void Vect_times_const(Vector_t *vector, _FLOAT_ constant, Vector_t *result_vector);
void Vect_copy_from_to(Vector_t *vector_from, Vector_t *vector_to);
void Vect_cross_multiply(Vector_t *vector_1, Vector_t *vector_2, Vector_t *resultvector);
void Vect_sum_up_list_of_vectors(Vector_t *vectorlist, Vector_t *sum_vector, uint32_t listlength);
void Vect_set_vectorlist_to_value(Vector_t vectorlist[], uint32_t listlength, _FLOAT_ value);
_FLOAT_ *Vect_pointer_to_index(Vector_t *vect, uint8_t index);

#endif
