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


typedef uint16_t* (*Retrieve_function_f)(void *target);

#define NUM_OF_MEASUREMENTS_TAKEN 7
void calculate_average(uint16_t v[NUM_OF_MEASUREMENTS_TAKEN][4], uint16_t r[4]);
int16_t recalculate_joystickValue(uint16_t value, int16_t min, int16_t max, int16_t offset);


void TimeTasks_run(uint32_t ticks){
	static uint8_t count = 0;
	static uint32_t remembered_time_8_ms;
	static uint32_t remembered_time_32_ms;
	// it seems like it's at least necessary to send 16 bytes for a stable transmission
#define NUM_RFM75_BYTES 32
	static uint8_t sendbytes[NUM_RFM75_BYTES] = {0};

#define NUM_DRONE_TO_BASE_BYTES 33
	static uint8_t droneToBase_bytes[NUM_DRONE_TO_BASE_BYTES] = {0};
	droneToBase_bytes[NUM_DRONE_TO_BASE_BYTES - 1] = '\n';

	static uint16_t adc_values[NUM_OF_MEASUREMENTS_TAKEN][4];

#define NUM_BASE_TO_DRONE_BYTES ((4*3) + 2)
	static uint8_t baseToDrone_bytes[NUM_BASE_TO_DRONE_BYTES] = {0};

	Joysticks_get_newest_values(adc_values[count]);
	count++;
	if(count >= NUM_OF_MEASUREMENTS_TAKEN){
		count = 0;
	}

	if(overflow_save_diff_u32(ticks, remembered_time_8_ms) >= 8){
		remembered_time_8_ms = ticks;
		uint16_t averaged[4] = {0};
		calculate_average(adc_values, averaged);

		averaged[0] = (recalculate_joystickValue(averaged[0], 3, 4094, 2064) + 2048);
		averaged[1] = (recalculate_joystickValue(averaged[1], 5, 4093, 2063) + 2048);
		averaged[2] = (recalculate_joystickValue(averaged[2], 4080, 150, 2003) + 2048);
		Joystick_serialize_data(averaged, sendbytes);

		UART_STATUS_t status = UART_Receive(&DEBUG_UART, baseToDrone_bytes, NUM_BASE_TO_DRONE_BYTES);
		if(status == UART_STATUS_SUCCESS){
			LED_on(1);
			if((baseToDrone_bytes[0] == 1) &&
			   (baseToDrone_bytes[NUM_BASE_TO_DRONE_BYTES-1] == 10)){
				format_copy_u8_buf(&baseToDrone_bytes[1], &sendbytes[6],
									NUM_BASE_TO_DRONE_BYTES-2);

				format_set_u8_buf_to(0, baseToDrone_bytes, 14);
			}
		}

		CombinedReg_t creg = RFM75_Transmit_bytes(sendbytes,
													NUM_RFM75_BYTES,
													2000,
													droneToBase_bytes,
													true);
		format_set_u8_buf_to(0, &sendbytes[6], NUM_BASE_TO_DRONE_BYTES-2);
		if(creg.tx_data_sent){
			LED_toggle(0);
			LED_off(1);
			format_set_u8_buf_to(0, baseToDrone_bytes,
								NUM_BASE_TO_DRONE_BYTES);
		}
		remembered_time_32_ms = ticks;
		UART_Transmit(&DEBUG_UART, droneToBase_bytes, sizeof(droneToBase_bytes));
	}

	if(overflow_save_diff_u32(ticks, remembered_time_32_ms) >= 32){

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

/* Outputs a fix range from  (-2048) to (2047) */
int16_t recalculate_joystickValue(uint16_t value, int16_t min, int16_t max, int16_t offset){
	float val = value;
	float mif = min;
	float maf = max;
	float offs = offset;
	int16_t r = (val - offs)*((4095.0f)/(maf-mif));
	if(r >= 2047){
		return 2047;
	}else if(r <= (-2048)){
		return -2048;
	}else{
		return r;
	}
}
