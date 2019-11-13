/*
 * buttons.h
 *
 *  Created on: Jul 22, 2017
 *      Author: chocolate
 */

#ifndef SRC_APPLICATION_BUTTONS_BUTTONS_H_
#define SRC_APPLICATION_BUTTONS_BUTTONS_H_

#include "base.h"

typedef enum _button_{
	_BUTTON1,
	_BUTTON2,
}Hdw_button_t;

typedef enum _edge_type_{
	NO_EDGE,
	RISING_EDGE,
	FALLING_EDGE,
}Edge_t;

typedef struct _Button_{
	Hdw_button_t btn;
	bool laststate;
}Button_t;

void buttons_init(void);
uint32_t button_read(Hdw_button_t button);
Edge_t button_readEdge(Button_t *btn);

#endif /* SRC_APPLICATION_BUTTONS_BUTTONS_H_ */
