#include "vector_operations.h"

_STATIC_ int32_t sign(_FLOAT_ floatnum);
_STATIC_ _FLOAT_ abs_of_float(_FLOAT_ floatnum);

bool kron_delta(uint8_t ind_1,uint8_t ind_2){
    return ind_1 == ind_2;
}

_FLOAT_ Vect_read(const Vector_t *vect, uint8_t index){
	// Values from 1 - 3 
	return vect->v[index-1];
}

void Vect_write(Vector_t *vect, uint8_t index, _FLOAT_ value){
	vect->v[index-1] = value;
}

_FLOAT_ Vect_length(const Vector_t *vect){
	return (_FLOAT_)(sqrt(Vect_dot(vect,vect)));
}

_FLOAT_ Vect_dot(const Vector_t *vect_1, const Vector_t *vect_2){
	_FLOAT_ sum = 0;
	uint8_t i;
	for(i=1; i<=3;i++){
		sum += Vect_read(vect_1, i)*Vect_read(vect_2, i);
	}
	return sum;
}

void Vect_set_all_values_to(Vector_t *vect, _FLOAT_ value){
    Vect_write_three_values(vect, value, value, value);
}

void Vect_add_to(Vector_t *sum_vect, const Vector_t *vect){
    Vect_add(vect, sum_vect, sum_vect);
}

void Vect_add(const Vector_t *vect_1, const Vector_t *vect_2, Vector_t *sum_vect){
    Vect_write(sum_vect, 1, Vect_read(vect_1, 1) + Vect_read(vect_2, 1));
    Vect_write(sum_vect, 2, Vect_read(vect_1, 2) + Vect_read(vect_2, 2));
    Vect_write(sum_vect, 3, Vect_read(vect_1, 3) + Vect_read(vect_2, 3));
}

void Vect_uniform(const Vector_t *vector, Vector_t *uniformed_vector){
    Vect_copy_from_to(vector, uniformed_vector);
    Vect_times_const(uniformed_vector, 1/(Vect_length(vector)), uniformed_vector);    
}

void Vect_write_three_values(Vector_t *vector, _FLOAT_ value_1, _FLOAT_ value_2, _FLOAT_ value_3){
    Vect_write(vector, 1, value_1);
    Vect_write(vector, 2, value_2);
    Vect_write(vector, 3, value_3);
}

void Vect_times_const(const Vector_t *vector, _FLOAT_ constant, Vector_t* result_vector){
    Vect_write(result_vector, 1, Vect_read(vector, 1) * constant);
    Vect_write(result_vector, 2, Vect_read(vector, 2) * constant);
    Vect_write(result_vector, 3, Vect_read(vector, 3) * constant);
}

void Vect_copy_from_to(const Vector_t *vector_from, Vector_t *vector_to){
	uint8_t i;
	for(i=0; i<3;i++){
		vector_to->v[i] = vector_from->v[i];
	}
}

void Vect_cross_multiply(const Vector_t *vector_1, const Vector_t *vector_2, Vector_t *resultvector){
    Vect_write(resultvector, 1, vector_1->v[1] * vector_2->v[2] - vector_1->v[2] * vector_2->v[1]);
    Vect_write(resultvector, 2, vector_1->v[2] * vector_2->v[0] - vector_1->v[0] * vector_2->v[2]);
    Vect_write(resultvector, 3, vector_1->v[0] * vector_2->v[1] - vector_1->v[1] * vector_2->v[0]);
    
}

void Vect_sum_up_list_of_vectors(const Vector_t vectorlist[], Vector_t *sum_vector, uint32_t listlength){
	uint32_t i;
	Vect_set_all_values_to(sum_vector, 0.0);
	for(i=0; i<listlength; i++){
		Vect_add(&(vectorlist[i]), sum_vector, sum_vector);
	}
}

void Vect_set_vectorlist_to_value(Vector_t vectorlist[], uint32_t listlength, _FLOAT_ value){
	uint32_t i;

	for(i=0; i<listlength; i++){
		Vect_set_all_values_to(&(vectorlist[i]), value);
	}
}

_FLOAT_ *Vect_pointer_to_index(Vector_t *vect, uint8_t index){
		return &(vect->v[index-1]);
}




int32_t Vect_i32_read(const Vector_i32_t *vect, uint8_t index){
	return vect->v[index-1];
}

void Vect_i32_write(Vector_i32_t *vect, uint8_t index, int32_t value){
	vect->v[index-1] = value;
}

void Vect_i32_set_all_values_to(Vector_i32_t *vect, int32_t value){
	Vect_i32_write_three_values(vect, value, value, value);
}

void Vect_i32_add_to(Vector_i32_t *sum_vect, const Vector_i32_t *vect){
	Vect_i32_add(vect, sum_vect, sum_vect);
}

void Vect_i32_add(const Vector_i32_t *vect_1, const Vector_i32_t *vect_2, Vector_i32_t *sum_vect){
	Vect_i32_write(sum_vect, 1, Vect_i32_read(vect_1, 1) + Vect_i32_read(vect_2, 1));
	Vect_i32_write(sum_vect, 2, Vect_i32_read(vect_1, 2) + Vect_i32_read(vect_2, 2));
	Vect_i32_write(sum_vect, 3, Vect_i32_read(vect_1, 3) + Vect_i32_read(vect_2, 3));
}

void Vect_i32_write_three_values(Vector_i32_t *vector, int32_t value_1, int32_t value_2, int32_t value_3){
	Vect_i32_write(vector, 1, value_1);
	Vect_i32_write(vector, 2, value_2);
	Vect_i32_write(vector, 3, value_3);
}

void Vect_i32_times_const(const Vector_i32_t *vector, int32_t constant, Vector_i32_t* result_vector){
	Vect_i32_write(result_vector, 1, Vect_i32_read(vector, 1) * constant);
	Vect_i32_write(result_vector, 2, Vect_i32_read(vector, 2) * constant);
	Vect_i32_write(result_vector, 3, Vect_i32_read(vector, 3) * constant);
}

void Vect_i32_copy_from_to(const Vector_i32_t *vector_from, Vector_i32_t *vector_to){
	uint8_t i;
	for(i=0; i<3;i++){
		vector_to->v[i] = vector_from->v[i];
	}
}

void Vect_i32_cross_multiply(const Vector_i32_t *vector_1, const Vector_i32_t *vector_2, Vector_i32_t *resultvector){
	Vect_i32_write(resultvector, 1, vector_1->v[1] * vector_2->v[2] - vector_1->v[2] * vector_2->v[1]);
	Vect_i32_write(resultvector, 2, vector_1->v[2] * vector_2->v[0] - vector_1->v[0] * vector_2->v[2]);
	Vect_i32_write(resultvector, 3, vector_1->v[0] * vector_2->v[1] - vector_1->v[1] * vector_2->v[0]);
	
}

void Vect_transform_float_to_i32_with_limits(const Vector_t *float_vect, Vector_i32_t *si_vect, uint8_t resolution, _FLOAT_ limit){
	/*
	  Linear tranformaton from float to int32 vector 
	 */
	uint8_t i;
	
	_FLOAT_ x;
	_FLOAT_ dx = abs_of_float(limit);
	_FLOAT_ dy = (1<<(resolution - 1));
	_FLOAT_ m = dy/dx;
	
	for(i=1; i <= 3; i++){
		x = Vect_read(float_vect, i);
		if(abs_of_float(x) > dx){
			Vect_i32_write(si_vect, i, (int32_t)(sign(x)*dy));
		}else{
			Vect_i32_write(si_vect, i, (int32_t)(x*m));			
		}
	}
}

_STATIC_ _FLOAT_ abs_of_float(_FLOAT_ floatnum){
	if(floatnum > 0){
		return floatnum;
	} else {
		return floatnum*(-1);
	}
}

_STATIC_ int32_t sign(_FLOAT_ floatnum){
	return ((floatnum > 0) - (floatnum < 0));
}
