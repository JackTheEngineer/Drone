/*
 * byte_formatting.c
 *
 *  Created on: Oct 24, 2017
 *      Author: jakov
 */

#include "byte_manip.h"

void format_u64_to_u8buf(uint64_t input_u64, uint8_t output_buf_pu8[]){
	output_buf_pu8[0] = (uint8_t)input_u64;
	output_buf_pu8[1] = (uint8_t)(input_u64 >> 8);
	output_buf_pu8[2] = (uint8_t)(input_u64 >> 16);
	output_buf_pu8[3] = (uint8_t)(input_u64 >> 24);
	output_buf_pu8[4] = (uint8_t)(input_u64 >> 32);
	output_buf_pu8[5] = (uint8_t)(input_u64 >> 40);
	output_buf_pu8[6] = (uint8_t)(input_u64 >> 48);
	output_buf_pu8[7] = (uint8_t)(input_u64 >> 56);
}

void format_u64_to_u8buf_highbyte_first(uint64_t input_u64, uint8_t output_buf_pu8[]){
	output_buf_pu8[7] = (uint8_t)input_u64;
	output_buf_pu8[6] = (uint8_t)(input_u64 >> 8);
	output_buf_pu8[5] = (uint8_t)(input_u64 >> 16);
	output_buf_pu8[4] = (uint8_t)(input_u64 >> 24);
	output_buf_pu8[3] = (uint8_t)(input_u64 >> 32);
	output_buf_pu8[2] = (uint8_t)(input_u64 >> 40);
	output_buf_pu8[1] = (uint8_t)(input_u64 >> 48);
	output_buf_pu8[0] = (uint8_t)(input_u64 >> 56);
}
void format_u64_to_u16buf(uint64_t input_u64, uint16_t output_buf_pu16[]){
	output_buf_pu16[0] = (uint16_t)input_u64;
	output_buf_pu16[1] = (uint16_t)(input_u64 >> 16);
	output_buf_pu16[2] = (uint16_t)(input_u64 >> 32);
	output_buf_pu16[3] = (uint16_t)(input_u64 >> 48);
}
void format_u64_to_u16buf_highbyte_first(uint64_t input_u64, uint16_t output_buf_pu16[]){
	output_buf_pu16[3] = (uint16_t)input_u64;
	output_buf_pu16[2] = (uint16_t)(input_u64 >> 16);
	output_buf_pu16[1] = (uint16_t)(input_u64 >> 32);
	output_buf_pu16[0] = (uint16_t)(input_u64 >> 48);
}

void format_u32_to_u8buf(uint32_t input_u32, uint8_t output_buf_pu8[]){
	output_buf_pu8[0] = (uint8_t)input_u32;
	output_buf_pu8[1] = (uint8_t)(input_u32 >> 8);
	output_buf_pu8[2] = (uint8_t)(input_u32 >> 16);
	output_buf_pu8[3] = (uint8_t)(input_u32 >> 24);
}

void format_u32_to_u8buf_highbyte_first(uint32_t input_u32, uint8_t output_buf_pu8[]){
	output_buf_pu8[3] = (uint8_t)input_u32;
	output_buf_pu8[2] = (uint8_t)(input_u32 >> 8);
	output_buf_pu8[1] = (uint8_t)(input_u32 >> 16);
	output_buf_pu8[0] = (uint8_t)(input_u32 >> 24);
}

void format_two_u28_to_u8buf(uint32_t input_first_u28,
							uint32_t input_second_u28,
							uint8_t output_buf_pu8[]){
	output_buf_pu8[0] = (uint8_t)input_first_u28;
	output_buf_pu8[1] = (uint8_t)(input_first_u28 >> 8);
	output_buf_pu8[2] = (uint8_t)(input_first_u28 >> 16);
	output_buf_pu8[3] = (uint8_t)((input_first_u28 >> 24) & 0x0F);
	output_buf_pu8[3] |= (uint8_t)((input_second_u28 & 0xF) << 4);
	output_buf_pu8[4] = (uint8_t)(input_second_u28 >> 4);
	output_buf_pu8[5] = (uint8_t)(input_second_u28 >> 12);
	output_buf_pu8[6] = (uint8_t)(input_second_u28 >> 20);
}

void format_u8buf_to_two_u28(uint8_t input_buf_pu8[],
		uint32_t *first_pu28,
		uint32_t *second_pu28){
	if((first_pu28 != NULL) && (second_pu28 != NULL)){
		*first_pu28 = (uint32_t)(input_buf_pu8[0]) |
						(((uint32_t)input_buf_pu8[1]) << 8) |
						(((uint32_t)input_buf_pu8[2]) << 16) |
						(((uint32_t)input_buf_pu8[3] & 0xF) << 24);
		*second_pu28 = (uint32_t)((input_buf_pu8[3] & 0xF0) >> 4) |
						(((uint32_t)input_buf_pu8[4]) << 4) |
						(((uint32_t)input_buf_pu8[5]) << 12) |
						(((uint32_t)input_buf_pu8[6]) << 20);
	}
}

void format_u16_to_u8buf(uint16_t input_u16, uint8_t *output_buf_pu8){
	output_buf_pu8[0] = (uint8_t)input_u16;
	output_buf_pu8[1] = (uint8_t)(input_u16 >> 8);
}

void format_u16buf_to_u8buf(uint16_t *inputbuf_u16, uint32_t inputlen, uint8_t *output_buf_pu8){
	for(uint32_t i=0; i < inputlen; i++){
		format_u16_to_u8buf(inputbuf_u16[i], &output_buf_pu8[i*2]);
	}
}

void format_u16_to_u8_highbyte_first(uint16_t input_u16, uint8_t *output_buf_pu8){
	output_buf_pu8[0] = (uint8_t)(input_u16 >> 8);
	output_buf_pu8[1] = (uint8_t)input_u16;
}

void format_four_u16_to_u8buf(uint16_t *input_buf,
		uint8_t output_buf_pu8[]){
	output_buf_pu8[0] = (uint8_t)(input_buf[0] >> 0) & 0xFF;
	output_buf_pu8[1] = (uint8_t)(input_buf[0] >> 8) & 0xFF;
	output_buf_pu8[2] = (uint8_t)(input_buf[1] >> 0) & 0xFF;
	output_buf_pu8[3] = (uint8_t)(input_buf[1] >> 8) & 0xFF;
	output_buf_pu8[4] = (uint8_t)(input_buf[2] >> 0) & 0xFF;
	output_buf_pu8[5] = (uint8_t)(input_buf[2] >> 8) & 0xFF;
	output_buf_pu8[6] = (uint8_t)(input_buf[3] >> 0) & 0xFF;
	output_buf_pu8[7] = (uint8_t)(input_buf[3] >> 8) & 0xFF;
}

void format_four_u12_to_u8buf(uint16_t * input_pu16,
							uint8_t * output_buf_pu8){
	format_two_u12_to_u8buf(input_pu16[0], input_pu16[1], &output_buf_pu8[0]);
	format_two_u12_to_u8buf(input_pu16[2], input_pu16[3], &output_buf_pu8[3]);
}

void format_two_u12_to_u8buf(uint16_t zero_pos_val_u12,
		uint16_t first_pos_val_u12,
		uint8_t *BufferOut_pu8){
	BufferOut_pu8[0] = (uint8_t)(zero_pos_val_u12 & 0xFF);
	BufferOut_pu8[1] = (uint8_t)(zero_pos_val_u12 >> 8) & 0xF;
	BufferOut_pu8[1] |= (uint8_t)((first_pos_val_u12) << 4) & 0xF0;
	BufferOut_pu8[2] = (uint8_t)(first_pos_val_u12 >> 4) & 0xFF;
}

void format_u8buf_to_four_ui12(uint8_t input_buf_pu8[],
		uint16_t *tgt){
	format_u8buf_to_two_ui12(input_buf_pu8, &tgt[0], &tgt[1]);
	format_u8buf_to_two_ui12(&input_buf_pu8[3], &tgt[2], &tgt[3]);
}

void format_u8buf_to_two_ui12(uint8_t input_buf_pu8[],
		uint16_t *zero_pos_pu16,
		uint16_t *first_pos_pu16){
	if((zero_pos_pu16 != NULL) &&
		(first_pos_pu16 != NULL)){

		*zero_pos_pu16 = ((uint16_t)(input_buf_pu8[1] & 0xF) << 8)|((uint16_t)(input_buf_pu8[0]));
		*first_pos_pu16 = ((uint16_t)(input_buf_pu8[2]) << 4)| ((uint16_t)(input_buf_pu8[1] & 0xF0) >> 4);
	}
}

void format_u8buf_to_u64(uint8_t *buf_pu8, uint64_t *val_u64){
	*val_u64 = (uint64_t)(buf_pu8[0]) |
			(((uint64_t)buf_pu8[1]) << 8) |
			(((uint64_t)buf_pu8[2]) << 16) |
			(((uint64_t)buf_pu8[3]) << 24) |
			(((uint64_t)buf_pu8[4]) << 32) |
			(((uint64_t)buf_pu8[5]) << 40) |
			(((uint64_t)buf_pu8[6]) << 48) |
			(((uint64_t)buf_pu8[7]) << 56);
}

void format_u8buf_to_u64_highbyte_first(uint8_t *buf_pu8, uint64_t *val_u64){
	*val_u64 = (uint64_t)(buf_pu8[7]) |
			(((uint64_t)buf_pu8[6]) << 8) |
			(((uint64_t)buf_pu8[5]) << 16) |
			(((uint64_t)buf_pu8[4]) << 24) |
			(((uint64_t)buf_pu8[3]) << 32) |
			(((uint64_t)buf_pu8[2]) << 40) |
			(((uint64_t)buf_pu8[1]) << 48) |
			(((uint64_t)buf_pu8[0]) << 56);
}

uint32_t format_u8buf_to_u32(uint8_t buf_pu8[]){
	return 	(uint32_t)(buf_pu8[0]) |
			(((uint32_t)buf_pu8[1]) << 8) |
			(((uint32_t)buf_pu8[2]) << 16) |
			(((uint32_t)buf_pu8[3]) << 24);
}
uint32_t format_u8buf_to_u32_highbyte_first(uint8_t buf_pu8[]){
	return 	(uint32_t)(buf_pu8[3]) |
			(((uint32_t)buf_pu8[2]) << 8) |
			(((uint32_t)buf_pu8[1]) << 16) |
			(((uint32_t)buf_pu8[0]) << 24);
}

uint16_t format_u8buf_to_u16(uint8_t buf_pu8[]){
	return 	(uint16_t)(buf_pu8[0]) |
			(((uint16_t)buf_pu8[1]) << 8);
}

uint16_t format_u8buf_to_u16_highbyte_first(uint8_t buf_pu8[]){
	return 	(uint16_t)(buf_pu8[1]) |
			(((uint16_t)buf_pu8[0]) << 8);
}

void format_copy_u8_buf(uint8_t *from_pu8, uint8_t *to_pu8, uint32_t size){
	uint32_t i;
	for(i=0; i < size; i++){
		to_pu8[i] = from_pu8[i];
	}
}

void format_set_u8_buf_to(uint8_t value, uint8_t *set_to_pu8, uint32_t size){
	uint32_t i;
	for(i=0; i < size; i++){
		set_to_pu8[i] = value;
	}
}

void format_float_to_u8_buf(float f, uint8_t *buf){
	uint8_t *c = (uint8_t *) &f;
	for(uint8_t i=0; i<4; i++){
		buf[i] = c[i];
	}
}

void format_float_buf_to_u8_buf(float *floatbuf, uint32_t bufsize, uint8_t *targetbuf){
	for(uint32_t i=0; i < bufsize; i++){
		format_float_to_u8_buf(floatbuf[i], &targetbuf[i*4]);
	}
}

float format_u8_buf_to_float(uint8_t *u8_buf){
	float *ptr = (float *)u8_buf;
	return *ptr;
}

void format_u8_buf_to_float_buf(uint8_t *u8_buf, uint32_t floatbufsize, float *floatbuf){
	for(uint32_t i=0; i < floatbufsize; i++){
		floatbuf[i] = format_u8_buf_to_float(&u8_buf[i*4]);
	}
}

void copy_u8_buf(uint8_t *from, uint8_t *to, uint32_t count){
	for(uint32_t i; i < count; i++){
		to[i] = from[i];
	}
}

uint32_t overflow_save_diff_u32(uint32_t minuend, 
		uint32_t subtrahend){
	uint32_t diff_time;
	if (minuend < subtrahend) {
		diff_time = 0xFFFFFFFF - subtrahend + minuend;
	}else{
		diff_time = minuend - subtrahend;
	}
	return diff_time;
}
