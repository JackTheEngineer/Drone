#ifndef _QUATERNIONS_H__
#define _QUATERNIONS_H__

#include "base.h"

typedef struct Quaternion{
	_FLOAT_ q[4];
}Quaternion_t;

void Quat_mult(Quaternion_t *q1, Quaternion_t *q2, Quaternion_t *resultQ);
void Quat_copy(Quaternion_t *from, Quaternion_t *to);
void Quat_conjugate(Quaternion_t *q, Quaternion_t *res);


#endif /* _QUATERNIONS_H__ */
