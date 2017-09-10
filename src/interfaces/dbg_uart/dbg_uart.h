/*
 * dbg_uart.h
 *
 *  Created on: Aug 31, 2017
 *      Author: chocolate
 */

#ifndef SRC_INTERFACES_DBG_UART_DBG_UART_H_
#define SRC_INTERFACES_DBG_UART_DBG_UART_H_

#include "base.h"
#include "uart.h"

void DBG_Uart_send_num(const UART_t *const handle, uint32_t number);

#endif /* SRC_INTERFACES_DBG_UART_DBG_UART_H_ */
