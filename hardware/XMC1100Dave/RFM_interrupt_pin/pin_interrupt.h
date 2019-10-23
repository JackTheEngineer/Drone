#ifndef RFM_PIN_INTERRUPT
#define RFM_PIN_INTERRUPT

#include "base.h"
#include <xmc_gpio.h>
#include <xmc_eru.h>

/* This is the pin interrupt handler */
#define RFM75_PinInterruptHandler ERU0_0_IRQHandler

typedef struct PIN_INTERRUPT
{
	XMC_ERU_t *eru;  /**< Mapped ERU module */
	XMC_GPIO_PORT_t *port;  /**< Mapped port number */
	uint8_t pin; /*< Mapped pin number */
	XMC_GPIO_CONFIG_t gpio_config;   /**< Initializes the input pin characteristics */
	XMC_ERU_ETL_CONFIG_t etl_config;  /**< reference to ERUx_ETLy (x = [0..1], y = [0..4])
					     module configuration */
	IRQn_Type IRQn;       /**< Mapped NVIC Node */
	uint8_t irq_priority; 	  /**< Node Interrupt Priority */

	uint8_t etl; /*< ETLx channel (x = [0..3])*/
	uint8_t ogu; /*< OGUy channel (y = [0..3])*/
	bool enable_at_init;  /**< Interrupt enable for Node at initialization*/
} PIN_INTERRUPT_t;

const PIN_INTERRUPT_t RFM75_INTERRUPT_PIN;

void PIN_INTERRUPT_Init(const PIN_INTERRUPT_t *const handle);

#endif /* RFM_PIN_INTERRUPT */
