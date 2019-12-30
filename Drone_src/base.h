
#ifndef BASE_H
#define BASE_H

#include <stdint.h>
#include <stdbool.h>
#include <math.h>

#include "target_dep.h"

#define SQR(x) ((x)*(x))

#ifndef _STATIC_
	#define _STATIC_ static
#endif

#ifndef _INLINE_
	#define _INLINE_ inline
#endif

#ifndef _FLOAT_
	#define _FLOAT_ float
#endif

#define TWO_TO_N_MIN_ONE(n) ((1<<(n)) - 1)

#define MAX(x,y) (((x)>(y))?(x):(y))

#define GLUE(x,y) x##y
#define POINTER_TO_CONTAINER(type, name) type GLUE(name,_container); type *name = &(GLUE(name,_container))
#define STATIC_POINTER_TO_CONTAINER(type, name) static type GLUE(name,_container); static type *name = &(GLUE(name,_container))

#endif
