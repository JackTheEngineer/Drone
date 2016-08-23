#include "vector_operations.h"

bool kron_delta(uint8_t ind_1,uint8_t ind_2){
    return ind_1 == ind_2;
}

/* 
 * Accepting index from 1 - 3
 */
double Vect_read(Vector_t *vect, uint8_t index){
    switch(index){
        case 1:
            return vect->x;
            break;
        case 2: 
            return vect->y;
            break;
        case 3:
            return vect->z;
            break;
        default:
            return 0;
            break;
    }
}

void Vect_write(Vector_t *vect, uint8_t index, double value){
    switch(index){
    case 1: 
        vect->x = value;
        break;
    case 2:
        vect->y = value;
        break;
    case 3:
        vect->z = value;
        break;
    default:
        break;
    }
}

double Vect_length(Vector_t *vect){
    return (double)(sqrt(Vect_dot(vect,vect)));
}

double Vect_dot(Vector_t *vect_1, Vector_t *vect_2){
    double sum = 0;
    uint8_t i;
    for(i=1; i<=3;i++){
        sum += Vect_read(vect_1, i)*Vect_read(vect_2, i);
    }
    return sum;
}

void Vect_set_all_values_to(Vector_t *vect, double value){
    Vect_write_three_values(vect, value, value, value);
}

void Vect_add_to(Vector_t *sum_vect, Vector_t *vect){
    Vect_add(vect, sum_vect, sum_vect);
}

void Vect_add(Vector_t *vect_1, Vector_t *vect_2, Vector_t *sum_vect){
    Vect_write(sum_vect, 1, Vect_read(vect_1, 1) + Vect_read(vect_2, 1));
    Vect_write(sum_vect, 2, Vect_read(vect_1, 2) + Vect_read(vect_2, 2));
    Vect_write(sum_vect, 3, Vect_read(vect_1, 3) + Vect_read(vect_2, 3));
}

void Vect_uniform(Vector_t *vector, Vector_t *uniformed_vector){
    Vect_copy_from_to(vector, uniformed_vector);
    Vect_multiply(uniformed_vector, 1/(Vect_length(vector)), uniformed_vector);    
}

void Vect_write_three_values(Vector_t *vector, double value_1, double value_2, double value_3){
    Vect_write(vector, 1, value_1);
    Vect_write(vector, 2, value_2);
    Vect_write(vector, 3, value_3);
}

void Vect_multiply(Vector_t *vector, double constant, Vector_t* result_vector){
    Vect_write(result_vector, 1, Vect_read(vector, 1) * constant);
    Vect_write(result_vector, 2, Vect_read(vector, 2) * constant);
    Vect_write(result_vector, 3, Vect_read(vector, 3) * constant);
}

void Vect_copy_from_to(Vector_t *vector_from, Vector_t *vector_to){
    vector_to->x = vector_from->x;
    vector_to->y = vector_from->y;
    vector_to->z = vector_from->z;
}

void Vect_cross_multiply(Vector_t *vector_1, Vector_t *vector_2, Vector_t *resultvector){
    Vect_write(resultvector, 1, vector_1->y * vector_2->z - vector_1->z * vector_2->y);
    Vect_write(resultvector, 2, vector_1->z * vector_2->x - vector_1->x * vector_2->z);
    Vect_write(resultvector, 3, vector_1->x * vector_2->y - vector_1->y * vector_2->x);
    
}

void Vect_sum_up_list_of_vectors(Vector_t *vectorlist, Vector_t *sum_vector, uint8_t listlength){
	uint8_t i;
	for(i=0; i<listlength; i++){
		Vect_add(&(vectorlist[i]), sum_vector, sum_vector);
	}
}

void Vect_set_vectorlist_to_value(Vector_t vectorlist[], uint32_t listlength, double value){
	uint32_t i;

	for(i=0; i<listlength; i++){
		Vect_set_all_values_to(&(vectorlist[i]), value);
	}
}

double *Vect_pointer_to_index(Vector_t *vect, uint8_t index){
	switch(index){
	case 1:
		return &(vect->x);
		break;
	case 2:
		return &(vect->y);
		break;
	case 3:
		return &(vect->z);
		break;
	default:
		return 0;
	}
}
