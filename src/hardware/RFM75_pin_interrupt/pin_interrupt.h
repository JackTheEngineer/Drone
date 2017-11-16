
#ifndef PIN_INTERRUPT_H
#define PIN_INTERRUPT_H

#include <xmc_gpio.h>
#include <xmc_eru.h>

#define PinInterruptHandler ERU0_3_IRQHandler

typedef enum PIN_INTERRUPT_STATUS
{
  PIN_INTERRUPT_STATUS_SUCCESS = 0U,        /**<APP initialization is success */
  PIN_INTERRUPT_STATUS_FAILURE = 1U         /**<APP initialization is failure */
} PIN_INTERRUPT_STATUS_t;

typedef enum PIN_INTERRUPT_EDGE
{
  PIN_INTERRUPT_EDGE_NONE = 0U, /**< no event enabled */
  PIN_INTERRUPT_EDGE_RISING = 1U,   /**< detection of rising edge generates the event */
  PIN_INTERRUPT_EDGE_FALLING = 2U,  /**< detection of falling edge generates the event */
  PIN_INTERRUPT_EDGE_BOTH = 3U      /**< detection of either edges generates the event */
} PIN_INTERRUPT_EDGE_t;

typedef struct PIN_INTERRUPT
{
  XMC_ERU_t *eru;  /**< Mapped ERU module */
  XMC_GPIO_PORT_t *port;  /**< Mapped port number */
  XMC_GPIO_CONFIG_t gpio_config;   /**< Initializes the input pin characteristics */
  XMC_ERU_ETL_CONFIG_t etl_config;  /**< reference to ERUx_ETLy (x = [0..1], y = [0..4])
                                                      module configuration */
  IRQn_Type IRQn;       /**< Mapped NVIC Node */
  uint8_t irq_priority; 	  /**< Node Interrupt Priority */
  uint8_t irq_subpriority;
  uint8_t etl; /*< ETLx channel (x = [0..3])*/
  uint8_t ogu; /*< OGUy channel (y = [0..3])*/
  uint8_t pin; /*< Mapped pin number */
} PIN_INTERRUPT_t;

const PIN_INTERRUPT_t RFM75_PIN_INTERRUPT;

void PIN_INTERRUPT_Init(const PIN_INTERRUPT_t *const handle);

#endif /* End of PIN_INTERRUPT_H */
