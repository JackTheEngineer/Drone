
#include "ADC_conversion_decoding.h"
#include "adc_measurement.h"

#define NUM_OF_MEASURED_CHANNELS 4

void Joys_BlockingADC_Measurement(uint16_t *results_to_be_filled)
{
	uint32_t result;
	uint16_t measurement_result=0;
	uint8_t channel_num=0;
	bool all_channels_ready = false;
	bool _all_channels_valid = false;
	ADC_result_t valid_results[NUM_OF_MEASURED_CHANNELS] = {{0}};
	const uint8_t required_channels[NUM_OF_MEASURED_CHANNELS] = {1,3,0,5};


	while(!all_channels_ready){
		result = ADC_MEASUREMENT_GetGlobalDetailedResult();
		if((bool)((result & VADC_GLOBRES_VF_Msk) >> VADC_GLOBRES_VF_Pos))
		{
			channel_num = (result & VADC_GLOBRES_CHNR_Msk) >> VADC_GLOBRES_CHNR_Pos;
			measurement_result = (result & VADC_GLOBRES_RESULT_Msk);
			_all_channels_valid = true;
			for(uint8_t i = 0; i < NUM_OF_MEASURED_CHANNELS; i++){
				if(required_channels[i] == channel_num){
					valid_results[i].channel_num = channel_num;
					valid_results[i].conversion_result = measurement_result;
					valid_results[i].valid = true;
				}
				_all_channels_valid &= valid_results[i].valid;
			}
			all_channels_ready = _all_channels_valid;
		}
	}
	for(uint8_t i = 0; i < NUM_OF_MEASURED_CHANNELS; i++){
		results_to_be_filled[i] = valid_results[i].conversion_result;
	}
}
