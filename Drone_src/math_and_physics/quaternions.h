#ifndef _QUATERNIONS_H__
#define _QUATERNIONS_H__

#include "base.h"

typedef struct Quaternion{
	_FLOAT_ q[4];
}Quaternion_t;

void Quat_mult(Quaternion_t *q1, Quaternion_t *q2, Quaternion_t *resultQ);
void Quat_copy(Quaternion_t *from, Quaternion_t *to);
void Quat_conjugate(Quaternion_t *q, Quaternion_t *res);

static inline void Quat_write_all(Quaternion_t *q, _FLOAT_ q0, _FLOAT_ q1, _FLOAT_ q2, _FLOAT_ q3){
	q->q[0] = q0;
	q->q[1] = q1;
	q->q[2] = q2;
	q->q[3] = q3;
}


#endif /* _QUATERNIONS_H__ */
