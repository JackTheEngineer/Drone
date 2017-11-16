/*
 * tick_.h
 *
 *  Created on: Jul 20, 2017
 *      Author: chocolate
 */

#ifndef SRC_HARDWARE_SYSTICKINTERRUPT_TICK__H_
#define SRC_HARDWARE_SYSTICKINTERRUPT_TICK__H_

#include "base.h"

void TIMER_Init(void);
void TIMER_Start(void);
void TIMER_Stop(void);
void TIMER_ClearEvent(void);
void TIMER_Clear(void);

#endif /* SRC_HARDWARE_SYSTICKINTERRUPT_TICK__H_ */
