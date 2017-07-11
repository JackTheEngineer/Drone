/*
 * timetasks.h
 *
 *  Created on: Jul 11, 2017
 *      Author: chocolate
 */

#ifndef SRC_RTE_TIMETASKS_H_
#define SRC_RTE_TIMETASKS_H_

#include "base.h"

#define TICK_TIME_MS 5
#define MS_100 (100/TICK_TIME_MS)

void Run_Next_Task(uint32_t *ticks);
void Tick_Interrupt_Init(void);


#endif /* SRC_RTE_TIMETASKS_H_ */
