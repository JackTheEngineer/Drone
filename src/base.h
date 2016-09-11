
#ifndef BASE_H
#define BASE_H

#include <stdbool.h>
#include <stdint.h>

#define _STATIC_ static
#define _INLINE_ inline

// The GLUE macro is needed because of "macro double evaluation" , google it.
#define GLUE(x,y) x##y
#define POINTER_TO_CONTAINER(type, name) type GLUE(name,_container); type *name = &(GLUE(name,_container))


#endif
