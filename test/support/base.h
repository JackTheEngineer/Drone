
#ifndef BASE_H
#define BASE_H

#include <stdbool.h>
#include <stdio.h>
#include <math.h>
;
typedef unsigned char uint8_t;
typedef signed char int8_t;
typedef short unsigned int uint16_t;
typedef short signed int int16_t;
typedef unsigned int uint32_t;
typedef signed int int32_t;
typedef long unsigned int uint64_t;
typedef long signed int int64_t;

#define _STATIC_
#define _INLINE_ 

#define SQR(x) ((x)*(x))

#define GLUE(x,y) x##y
#define POINTER_TO_CONTAINER(type, name) type GLUE(name,_container); type *name = &(GLUE(name,_container));

#endif
