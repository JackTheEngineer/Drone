/*
 * joystick.c
 *
 *  Created on: Jun 7, 2018
 *      Author: jakov
 */

#include "joystick.h"
#include "adc_measurement.h"
#include "ADC_conversion_decoding.h"
#include "hardware.h"

void Joystick_Init(void){
	// ADC_MEASUREMENT_Init(&JOYSTICK_ADC);
}

void Joysticks_get_current_values(Joystick_t *left_joystick, Joystick_t *right_joystick){
	uint16_t results[NUM_OF_MEASURED_CHANNELS];
	ADC_MEASUREMENT_StartConversion(&JOYSTICK_ADC);

	Joys_BlockingADC_Measurement(results);

	left_joystick->horizontal = results[0];
	left_joystick->vertical = results[1];
	right_joystick->horizontal =  results[2];
	right_joystick->vertical =  results[3];
}

void Joystick_serialize_data(Joystick_t *l_joystick, Joystick_t *r_joystick, uint8_t *sendbytes){
	sendbytes[0] = (l_joystick->horizontal) & 0xFF;
	sendbytes[1] = (l_joystick->horizontal >> 8) & 0xFF;
	sendbytes[2] = (l_joystick->vertical) & 0xFF;
	sendbytes[3] = (l_joystick->vertical >> 8) & 0xFF;
	sendbytes[4] = (r_joystick->horizontal) & 0xFF;
	sendbytes[5] = (r_joystick->horizontal >> 8) & 0xFF;
	sendbytes[6] = (r_joystick->vertical) & 0xFF;
	sendbytes[7] = (r_joystick->vertical >> 8) & 0xFF;
}


