
#ifndef BASE_H
#define BASE_H

#include <stdint.h>
#include <stdbool.h>
#include <math.h>

#include "target_dep.h"

#define SQR(x) ((x)*(x))

#define _STATIC_ static
#define _INLINE_ inline
#define _FLOAT_ float

#define BINARY_ONES(n) ((1<<(n)) - 1)

#define GLUE(x,y) x##y
#define POINTER_TO_CONTAINER(type, name) type GLUE(name,_container); type *name = &(GLUE(name,_container))
#define STATIC_POINTER_TO_CONTAINER(type, name) static type GLUE(name,_container); static type *name = &(GLUE(name,_container))

typedef enum _Direction_{
	UP,
	DOWN,
}Direction_t;

#endif
