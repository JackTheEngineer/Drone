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

#define NUM_OF_MEASUREMENTS_TAKEN 10


typedef uint16_t* (*Retrieve_function_f)(void *target);

void calculate_average(uint16_t v[NUM_OF_MEASUREMENTS_TAKEN][4], uint16_t r[4]);

uint16_t adc_values[NUM_OF_MEASUREMENTS_TAKEN][4];
uint8_t count = 0;

void TimeTasks_run(uint32_t ticks, OS_t *os){
	uint8_t sendbytes[32] = {0}; // it seems like it's at least necessary to send 16 bytes for a stable transmission :( bah.
	uint32_t length = 32;

	if((ticks % 1) == 0){
		Joysticks_get_newest_values(adc_values[count]);
		count++;
		if(count >= NUM_OF_MEASUREMENTS_TAKEN){
			count = 0;
		}
	}
	if((ticks % 20) == 0){
		uint16_t averaged[4];
		calculate_average(adc_values, averaged);
		Joystick_serialize_data(averaged, sendbytes);

		RFM75_turnOn();
		RFM75_CE_PIN_high();
		RFM75_Transmit_bytes(sendbytes,
							 length,
						     50,
						     true);
		RFM75_CE_PIN_low();
		RFM75_turnOff();
		LED_toggle();
	}
}

void calculate_average(uint16_t v[NUM_OF_MEASUREMENTS_TAKEN][4], uint16_t r[4]){
	for(uint8_t i=0; i<NUM_OF_MEASUREMENTS_TAKEN; i++){
		for(uint8_t j=0; j<4; j++){
			r[j] += v[i][j];
		}
	}
	for(uint8_t j=0; j<4; j++){
		r[j] /= NUM_OF_MEASUREMENTS_TAKEN;
	}
}
