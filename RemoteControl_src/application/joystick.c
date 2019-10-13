/*
 * joystick.c
 *
 *  Created on: Jun 7, 2018
 *      Author: jakov
 */

#include "joystick.h"
#include "adc_measurement.h"
#include "ADC_conversion_decoding.h"

extern bool new_result_available;
extern ADC_result_t latest_results[NUM_OF_MEASURED_CHANNELS];

void Joystick_Init(void){
	ADC_MEASUREMENT_Init(&ADC_MEASUREMENT_0);
}

void Joysticks_get_current_values(Joystick_t *left_joystick, Joystick_t *right_joystick){
	uint32_t count = 0;
	new_result_available = false;

    /* Enable Background Scan Request source IRQ */
    NVIC_EnableIRQ(ADC_MEASUREMENT_0.result_intr_handle->node_id);
	ADC_MEASUREMENT_StartConversion(&ADC_MEASUREMENT_0);


	/* new_result_avalilable is to true by ADC_Measurement_Handler IRQ in ADC_conversion_decoding */
	while((new_result_available == false) && (count < 10)){
		if(count == 5){
			ADC_MEASUREMENT_StartConversion(&ADC_MEASUREMENT_0); /*restarting the measurement if the adc got stuck */
		}
		count++;
		/* For whatever reason the ADC converter get's stuck every 5'th time,
		 * and does not enter the Measurement Interrupt. The Data stays the same.
		 *
		 * In the case of the remote control, this is not too relevant.
		 */
	}

	NVIC_DisableIRQ(ADC_MEASUREMENT_0.result_intr_handle->node_id);


	left_joystick->horizontal = latest_results[0].conversion_result;
	left_joystick->vertical = latest_results[1].conversion_result;
	right_joystick->horizontal =  latest_results[2].conversion_result;
	right_joystick->vertical =  latest_results[3].conversion_result;
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


