#ifndef FORCE_INJECTION
#define FORCE_INJECTION

#include "base.h"
#include "vector_operations.h"

void Inject_Force_into_simulation_in_earth_frame(Vector_t *force);
void Get_error_force_in_earth_frame(Vector_t *force);
void Inject_Moment_into_simulation_in_earth_frame(Vector_t *moment);      
void Get_error_moment_in_earth_frame(Vector_t *moment);

#endif /* FORCE_INJECTION */
