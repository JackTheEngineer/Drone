/*
 * os.h
 *
 *  Created on: Jul 23, 2017
 *      Author: chocolate
 */

#ifndef SRC_OS_H_
#define SRC_OS_H_

#include "buttons.h"

typedef struct _os_{
	Button_t *button_1;
	Button_t *button_2;
	int32_t *frequ_index;
}OS_t;


#endif /* SRC_OS_H_ */
