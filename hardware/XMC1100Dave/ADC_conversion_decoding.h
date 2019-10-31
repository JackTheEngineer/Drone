
#ifndef ADC_CONVERSION_DECODING_H
#define ADC_CONVERSION_DECODING_H

#include "base.h"

#define NUM_OF_MEASURED_CHANNELS 4

typedef struct detailed_result_struct
{
	uint8_t channel_num;
	uint16_t conversion_result;
}ADC_result_t;

bool checkMeasurementReady(void);

#endif /* ADC_CONVERSION_DECODING_H */
