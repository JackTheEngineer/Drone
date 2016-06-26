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
    double ind_1 = Vect_read(vect, 1);
    double ind_2 = Vect_read(vect, 2);
    double ind_3 = Vect_read(vect, 3);
    return (double)(sqrt(SQR(ind_1) + SQR(ind_2) + SQR(ind_3)));
}

void Vect_set_all_values_to(Vector_t *vect, double value){
    Vect_write_three_values(vect, value, value, value);
}

void Vect_add(Vector_t *vect_1, Vector_t *vect_2, Vector_t *sum_vect){
    Vect_write(sum_vect, 1, Vect_read(vect_1, 1) + Vect_read(vect_2, 1));
    Vect_write(sum_vect, 2, Vect_read(vect_1, 2) + Vect_read(vect_2, 2));
    Vect_write(sum_vect, 3, Vect_read(vect_1, 3) + Vect_read(vect_2, 3));
}

void Vect_uniform(Vector_t *vector, Vector_t *uniformed_vector){
    Vect_copy_from_to(vector, uniformed_vector);
    Vect_multiply(uniformed_vector, 1/(Vect_length(vector)));    
}

void Vect_write_three_values(Vector_t *vector, double value_1, double value_2, double value_3){
    Vect_write(vector, 1, value_1);
    Vect_write(vector, 2, value_2);
    Vect_write(vector, 3, value_3);
}

void Vect_multiply(Vector_t *vector, double constant){
    Vect_write(vector, 1, Vect_read(vector, 1) * constant);
    Vect_write(vector, 2, Vect_read(vector, 2) * constant);
    Vect_write(vector, 3, Vect_read(vector, 3) * constant);
}

void Vect_copy_from_to(Vector_t *vector_from, Vector_t *vector_to){
    vector_to->x = vector_from->x;
    vector_to->y = vector_from->y;
    vector_to->z = vector_from->z;
}
