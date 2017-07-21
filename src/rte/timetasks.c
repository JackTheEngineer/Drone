/*
 * timetasks.c
 *
 *  Created on: Jul 11, 2017
 *      Author: chocolate
 */
#include "timetasks.h"
#include "led_module.h"

volatile uint32_t volatile tick_count;

void TimeTasks_run(void){
	if((tick_count % TIME100MS) == 0){
			led_toggle(LED0);
	}
}

