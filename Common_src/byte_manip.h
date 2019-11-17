/*
 * byte_formatting.h
 *
 *  Created on: Oct 24, 2017
 *      Author: jakov
 */

#ifndef SOURCES_INTERFACE_BYTE_FORMATTING_BYTE_FORMATTING_H_
#define SOURCES_INTERFACE_BYTE_FORMATTING_BYTE_FORMATTING_H_

#include "base.h"

void format_u64_to_u8buf(uint64_t input_u64, uint8_t output_buf_pu8[]);
void format_u64_to_u8buf_highbyte_first(uint64_t input_u64, uint8_t output_buf_pu8[]);
void format_four_u16_to_u8buf(uint16_t *input_buf,
		uint8_t output_buf_pu8[]);
void format_four_u12_to_u8buf(uint16_t * input_pu16, uint8_t * output_buf_pu8);
uint32_t format_u8buf_to_u32(uint8_t buf_pu8[]);
void format_u8buf_to_u64(uint8_t *buf_pu8, uint64_t *val_u64);
void format_u8buf_to_u64_highbyte_first(uint8_t *buf_pu8, uint64_t *val_u64);
void format_u32_to_u8buf(uint32_t input_u32, uint8_t output_buf_pu8[]);
void format_u32_to_u8buf_highbyte_first(uint32_t input_u32, uint8_t output_buf_pu8[]);

void format_u16_to_u8buf(uint16_t input_u16, uint8_t *output_buf_pu8);
void format_u16_to_u8_highbyte_first(uint16_t input_u16, uint8_t *output_buf_pu8);

uint16_t format_u8buf_to_u16(uint8_t buf_pu8[]);
uint16_t format_u8buf_to_u16_highbyte_first(uint8_t buf_pu8[]);
void format_u64_to_u16buf(uint64_t input_u64, uint16_t output_buf_pu16[]);
void format_u64_to_u16buf_highbyte_first(uint64_t input_u64, uint16_t output_buf_pu16[]);

void format_u8buf_to_two_ui12(uint8_t input_buf_pu8[],
		uint16_t *zero_pos_pu16,
		uint16_t *first_pos_pu16);
void format_u8buf_to_four_ui12(uint8_t input_buf_pu8[],
				uint16_t *tgt);
void format_two_u12_to_u8buf(uint16_t zero_pos_val_u12,
		uint16_t first_pos_val_u12,
		uint8_t *BufferOut_pu8);
void format_two_u28_to_u8buf(uint32_t input_first_u28,
			     uint32_t input_second_u28,
			     uint8_t output_buf_pu8[]);
void format_u8buf_to_two_u28(uint8_t input_buf_pu8[],
			     uint32_t *first_pu28,
			     uint32_t *second_pu28);
void format_copy_u8_buf(uint8_t *from_u8, uint8_t *to_u8, uint32_t size);
void format_set_u8_buf_to(uint8_t value, uint8_t *set_to_pu8, uint32_t size);
void format_float_to_u8_buf(float f, uint8_t *buf);
void format_float_buf_to_u8_buf(float *floatbuf, uint32_t bufsize, uint8_t *targetbuf);

void copy_u8_buf(uint8_t *from, uint8_t *to, uint32_t count);

uint32_t overflow_save_diff_u32(uint32_t minuend,
		uint32_t subtrahend);

#endif /* SOURCES_INTERFACE_BYTE_FORMATTING_BYTE_FORMATTING_H_ */
