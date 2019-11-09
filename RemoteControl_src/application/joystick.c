/*
 * joystick.c
 *
 *  Created on: Jun 7, 2018
 *      Author: jakov
 */

#include "joystick.h"
#include "adc_measurement.h"
#include "ADC_conversion_decoding.h"
#include "byte_manip.h"
#include "hardware.h"

void Joysticks_get_newest_values(uint16_t results[NUM_OF_MEASURED_CHANNELS]){
	ADC_MEASUREMENT_StartConversion(&JOYSTICK_ADC);
	Joys_BlockingADC_Measurement(results);
}

void Joystick_serialize_data(uint16_t *joystick_data, uint8_t *targetbuf){
	format_four_u12_to_u8buf(joystick_data, targetbuf);
}


