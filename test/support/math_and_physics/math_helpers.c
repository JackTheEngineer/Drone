#include "math_helpers.h"


bool kron_delta(uint8_t ind_1,uint8_t ind_2){
    return ind_1 == ind_2;
}

/* 
 * Accepting index from 1 - 3
 */
double vect_val_of_ind(Vector_t *vect, uint8_t index){
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

double radius_of_vect(Vector_t *vect){
	return sqrt(SQR(vect->x) + SQR(vect->y) + SQR(vect->z));
}
