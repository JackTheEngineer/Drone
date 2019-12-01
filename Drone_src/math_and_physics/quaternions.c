#include "quaternions.h"

__attribute__((optimize("O2")))
void Quat_mult(Quaternion_t *q1, Quaternion_t *q2, Quaternion_t *resultQ){
	POINTER_TO_CONTAINER(Quaternion_t, temp);
	temp->q[0] = q1->q[0]*q2->q[0] \
			   - q1->q[1]*q2->q[1] \
			   - q1->q[2]*q2->q[2] \
			   - q1->q[3]*q2->q[3] ;

	temp->q[1] = q1->q[0]*q2->q[1] \
			   + q1->q[1]*q2->q[0] \
			   + q1->q[2]*q2->q[3] \
			   - q1->q[3]*q2->q[2];

	temp->q[2] = q1->q[0]*q2->q[2] \
			   - q1->q[1]*q2->q[3] \
			   + q1->q[2]*q2->q[0] \
			   + q1->q[3]*q2->q[1];

	temp->q[3] = q1->q[0]*q2->q[3] \
			   + q1->q[1]*q2->q[2] \
			   - q1->q[2]*q2->q[1] \
			   + q1->q[3]*q2->q[0];
	Quat_copy(temp, resultQ);

}

__attribute__((optimize("O2")))
void Quat_copy(Quaternion_t *from, Quaternion_t *to){
	for(uint8_t i=0; i < 4; i++){
		to->q[i] = from->q[i];
	}
}
