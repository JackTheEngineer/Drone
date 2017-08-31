/*
 * dbg_uart.c
 *
 *  Created on: Aug 31, 2017
 *      Author: chocolate
 */

#include "dbg_uart.h"
#include "stdio.h"

#define SENDNUM_BUFSIZE 6

void DBG_Uart_send_num(const UART_t *const handle, uint8_t number){
	uint8_t sendbuf[SENDNUM_BUFSIZE];
	snprintf((char*)sendbuf, SENDNUM_BUFSIZE, "%d\n", number);
	UART_Transmit(handle, &sendbuf[0], SENDNUM_BUFSIZE);
}


