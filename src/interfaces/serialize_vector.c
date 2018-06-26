/*
 * serialize_vector.c
 *
 *  Created on: Jun 14, 2018
 *      Author: jakov
 */



#include "vector_operations.h"
#include "byte_formatting.h"

#define SHORT_MAX 32767
#define SHORT_MIN -32768

_STATIC_ _INLINE_ int16_t cast_from_i32(int32_t val);

void serialize_vector_as_i16(Vector_i32_t *vect, uint8_t *buffer){

	for(uint8_t i = 0; i < 3; i++){
		format_u16_to_u8buf(cast_from_i32(vect->v[i]), &buffer[i*2]);
	}
}

void serialize_vector_as_i32(Vector_i32_t *vect, uint8_t *buffer){
	for(uint8_t i = 0; i < 3; i++){
		format_u32_to_u8buf(vect->v[i], &buffer[i*4]);
	}
}

_STATIC_ _INLINE_ int16_t cast_from_i32(int32_t val){
	int16_t helper;
	if((val <= SHORT_MAX) && (val >= SHORT_MIN)){
		helper = val;
	}else{
		if(helper > SHORT_MAX){
			helper = SHORT_MAX;
		}else if(helper < SHORT_MIN){
			helper = SHORT_MIN;
		}
	}
	return helper;
}
