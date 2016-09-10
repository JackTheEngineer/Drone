
#ifndef BASE_H
#define BASE_H

#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>
#include <math.h>

#define _STATIC_
#define _INLINE_ 

#define SQR(x) ((x)*(x))

#define GLUE(x,y) x##y
#define POINTER_TO_CONTAINER(type, name) type GLUE(name,_container); type *name = &(GLUE(name,_container));

#endif
