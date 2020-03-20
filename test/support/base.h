
#ifndef BASE_H
#define BASE_H

#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>
#include <math.h>

#include "target_dep.h"

#define _STATIC_
#define _INLINE_ 
#define _FLOAT_ float

#define SQR(x) ((x)*(x))
#define BINARY_ONES(n) ((1<<(n)) - 1)

#define GLUE(x,y) x##y
#define POINTER_TO_CONTAINER(type, name) type GLUE(name,_container); type *name = &(GLUE(name,_container));
#define STATIC_POINTER_TO_CONTAINER(type, name) static type GLUE(name,_container); static type *name = &(GLUE(name,_container))

#endif
