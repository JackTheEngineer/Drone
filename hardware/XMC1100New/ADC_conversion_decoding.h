
#ifndef ADC_CONVERSION_DECODING_H
#define ADC_CONVERSION_DECODING_H

#include "base.h"

#define NUM_OF_MEASURED_CHANNELS 4

typedef struct detailed_result_struct
{
	uint16_t conversion_result;
	uint8_t channel_num;
	bool valid;
}ADC_result_t;

void Joys_BlockingADC_Measurement(uint16_t *results_to_be_filled);

#endif /* ADC_CONVERSION_DECODING_H */
