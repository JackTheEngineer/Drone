
#include "ADC_conversion_decoding.h"
#include "adc_measurement.h"

#define NUM_OF_MEASURED_CHANNELS 4

_STATIC_ void copy_and_sort_ADC_results(ADC_result_t *from, ADC_result_t *to, uint32_t size);
_STATIC_ void copy_ADC_val(ADC_result_t *from, ADC_result_t *to);

ADC_result_t buffered_results[NUM_OF_MEASURED_CHANNELS];
ADC_result_t latest_results[NUM_OF_MEASURED_CHANNELS];

bool new_result_available;

void Adc_Measurement_Handler(void)
{
	static uint8_t index;
	uint32_t result;

	result = ADC_MEASUREMENT_GetGlobalDetailedResult();

	/* I wouldn't know why the interrupt handler should be called otherwise, 
	 * but it seems to be necessary to ask in the result for this bit ---.
	 * As so often during development i just copy pasted this code from 
	 * an Infineon Example.
	 */
	if((bool)(result >> VADC_GLOBRES_VF_Pos))
	{
		buffered_results[index].channel_num = (result & VADC_GLOBRES_CHNR_Msk) >> VADC_GLOBRES_CHNR_Pos;
		buffered_results[index].conversion_result = (result & VADC_GLOBRES_RESULT_Msk);
		index++;
		
		if(index > NUM_OF_MEASURED_CHANNELS){
			copy_and_sort_ADC_results(buffered_results, latest_results, NUM_OF_MEASURED_CHANNELS);
			index=0;
			new_result_available = true;
		}
	}
}

_STATIC_ void copy_and_sort_ADC_results(ADC_result_t *from, ADC_result_t *to, uint32_t size){
	for(uint32_t i = 0; i < size; i++){
		switch(from[i].channel_num){
		case 0: /* Those are the hardware channel numbers in the XMC1100 */
			copy_ADC_val(&from[i], &to[0]);
			break;
		case 1:
			copy_ADC_val(&from[i], &to[1]);
			break;
		case 2:
			copy_ADC_val(&from[i], &to[2]);
			break;
		case 4:
			copy_ADC_val(&from[i], &to[3]);
			break;
		default:
			break;
		}
	}
}

_STATIC_ void copy_ADC_val(ADC_result_t *from, ADC_result_t *to){
	to->channel_num = from->channel_num;
	to->conversion_result = from->conversion_result;
}
