
#ifndef PHYSICAL_HELPERS_H
#define PHYSICAL_HELPERS_H

#include "physical_definitions.h"

void physics_calculate_moment_of_inertia(Masspoint_t *masspoints, uint32_t number_of_masspoints, three_by_three_t *J);

#endif
