
#ifndef BASE_H
#define BASE_H

#include <stdbool.h>
#include <stdint.h>

#include "target_dep.h"

#define SQR(x) ((x)*(x))

#define _STATIC_ static
#define _INLINE_ inline
#define _FLOAT_ float

#define GLUE(x,y) x##y
#define POINTER_TO_CONTAINER(type, name) type GLUE(name,_container); type *name = &(GLUE(name,_container))


#endif
