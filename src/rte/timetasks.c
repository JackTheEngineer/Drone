/*
 * timetasks.c
 *
 *  Created on: Jul 11, 2017
 *      Author: chocolate
 */
#include "timetasks.h"
#include "led_module.h"
#include "motor_pwm.h"


#define FREQUS 40

static const uint32_t frequencies[FREQUS] = {
		10000,
		10500,
		11000,
		11500,
		12000,
		12500,
		13000,
		13500,
		14000,
		14500,
		15000,
		15500,
		16000,
		16500,
		17000,
		17500,
		18000,
		18500,
		19000,
		19500,
		20000,
		20500,
		21000,
		21500,
		22000,
		22500,
		23000,
		23500,
		24000,
		24500,
		25000,
		25500,
		26000,
		26500,
		27000,
		27500,
		28000,
		28500,
		29000,
		29500,
};

int32_t cut_index(int32_t *f, uint32_t bufsize){
	if(*f < 0){
		return bufsize - 1;
	}
	if(*f >= (bufsize)){
		return 0;
	}
	return *f;
}
void Frequ_down(int32_t *frequ_index){
	if(frequ_index == NULL){
		return;
	}
	int32_t frequ;
	*frequ_index = *frequ_index - 1;
	frequ = cut_index(frequ_index, FREQUS);

	Pin3_set_frequ(frequencies[frequ]);
	*frequ_index = frequ;
}
void Frequ_up(int32_t *frequ_index){
	if(frequ_index == NULL){
		return;
	}
	int32_t frequ;

	*frequ_index = *frequ_index + 1;
	frequ = cut_index(frequ_index, FREQUS);

	Pin3_set_frequ(frequencies[frequ]);
	*frequ_index = frequ;
}

void TimeTasks_run(uint32_t ticks, OS_t *os){
	if((ticks % TIME5MS) == 0){
		if(button_readEdge(os->button_1) == RISING_EDGE){
			Frequ_up(os->frequ_index);
		}
		if(button_readEdge(os->button_2) == RISING_EDGE){
			Frequ_down(os->frequ_index);
		}
	}
	if((ticks % TIME100MS) == 0){
		led_toggle(LED0);
	}
	if((ticks % TIME1S) == 0){
		led_toggle(LED1);
	}
}

