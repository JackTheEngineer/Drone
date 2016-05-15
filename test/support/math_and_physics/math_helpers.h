
#ifndef _MATH__HELPERS__
#define _MATH__HELPERS__

#include "base.h"
#include "physical_definitions.h"

double vect_val_of_ind(Vector_t *vect, uint8_t index);
bool kron_delta(uint8_t ind_1, uint8_t ind_2);
double radius_of_vect(Vector_t *vect);

#endif
