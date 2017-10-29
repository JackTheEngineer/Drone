/*
 * timetasks.h
 *
 *  Created on: Jul 11, 2017
 *      Author: chocolate
 */

#ifndef SRC_RTE_TIMETASKS_H_
#define SRC_RTE_TIMETASKS_H_

#include "base.h"
#include "tick_.h"
#include "os.h"

#define TIME5MS (uint32_t)5
#define TIME100MS (uint32_t)100
#define TIME1S (uint32_t)1000
#define TIME2S (uint32_t)2000


void TimeTasks_run(uint32_t ticks, OS_t *os);

#endif /* SRC_RTE_TIMETASKS_H_ */
