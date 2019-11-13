/*
 * timetasks.c
 *
 *  Created on: Jul 11, 2017
 *      Author: chocolate
 */
#include "timetasks.h"

#include "pin_pulse.h"
#include "RFM75_driver.h"
#include "delay.h"
#include "led.h"
#include "joystick.h"
#include "byte_manip.h"

#define NUM_OF_MEASUREMENTS_TAKEN 20

typedef uint16_t* (*Retrieve_function_f)(void *target);

void calculate_average(uint16_t v[NUM_OF_MEASUREMENTS_TAKEN][4], uint16_t r[4]);

uint16_t adc_values[NUM_OF_MEASUREMENTS_TAKEN][4];
uint8_t count = 0;

void TimeTasks_run(uint32_t ticks, OS_t *os){
	uint8_t sendbytes[32] = {0}; // it seems like it's at least necessary to send 16 bytes for a stable transmission :( bah.
	uint32_t length = 32;
	uint8_t rx_bytes[32] = {0};
	static uint32_t remembered_time_20_ms;

#define NUM_UART_BYTES (4 + 1)
	uint8_t uart_bytes[NUM_UART_BYTES];
	uart_bytes[NUM_UART_BYTES-1] = '\n';

	Joysticks_get_newest_values(adc_values[count]);
	count++;
	if(count >= NUM_OF_MEASUREMENTS_TAKEN){
		count = 0;
	}

	if(overflow_save_diff_u32(ticks, remembered_time_20_ms) >= 20){
		remembered_time_20_ms = ticks;
		uint16_t averaged[4] = {0};
		calculate_average(adc_values, averaged);
		Joystick_serialize_data(averaged, sendbytes);
		CombinedReg_t creg = RFM75_Transmit_bytes(sendbytes,
												length,
												4000,
												rx_bytes,
												true);
		format_u32_to_u8buf(creg.all, uart_bytes);
		if(creg.tx_data_sent){
			LED_toggle();
		}
		UART_Transmit(&DEBUG_UART, uart_bytes, NUM_UART_BYTES);
	}
}

void calculate_average(uint16_t v[NUM_OF_MEASUREMENTS_TAKEN][4], uint16_t r[4]){
	uint32_t acc[4] = {0};
	for(uint8_t i=0; i<NUM_OF_MEASUREMENTS_TAKEN; i++){
		for(uint8_t j=0; j<4; j++){
			acc[j] += v[i][j];
		}
	}
	for(uint8_t j=0; j<4; j++){
		r[j] = acc[j]/NUM_OF_MEASUREMENTS_TAKEN;
	}
}
