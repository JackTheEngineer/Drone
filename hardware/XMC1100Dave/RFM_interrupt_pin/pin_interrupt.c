#include "pin_interrupt.h"

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

void PIN_INTERRUPT_SetEdgeSensitivity(const PIN_INTERRUPT_t *const handle, const PIN_INTERRUPT_EDGE_t edge);
PIN_INTERRUPT_EDGE_t PIN_INTERRUPT_GetEdgeSensitivity(const PIN_INTERRUPT_t *const handle);

const PIN_INTERRUPT_t RFM75_INTERRUPT_PIN =
{
	.eru = XMC_ERU0,  /* ERU module 0 Mapped */
	.port = XMC_GPIO_PORT2,  /* PORT 2 Mapped */
	.pin = 9U, /* Mapped pin number */
	.gpio_config =
	{
		.input_hysteresis = XMC_GPIO_INPUT_HYSTERESIS_STANDARD,
		.mode = XMC_GPIO_MODE_INPUT_TRISTATE  /* Pin Characteristics */
	},
	.etl_config = 
	{
		.input_a = (uint32_t)XMC_ERU_ETL_INPUT_A0, /* Event input selection for A(0-3) */
		.input_b = (uint32_t)XMC_ERU_ETL_INPUT_B0, /* Event input selection for B(0-3) */
		.enable_output_trigger = (uint32_t)1,
		.edge_detection = (XMC_ERU_ETL_EDGE_DETECTION_t)PIN_INTERRUPT_EDGE_FALLING, /* Select the edge to convert as event */
		.output_trigger_channel = XMC_ERU_ETL_OUTPUT_TRIGGER_CHANNEL0, /* Select the source for event */
		.source = XMC_ERU_ETL_SOURCE_B
	},
	.IRQn = (IRQn_Type)3,  /* Mapped Interrupt Request Number */
	.irq_priority = 3, /* Priority of the Interrupt */
	.etl = 2U,  /* ERU ETL channel number */
	.ogu = 0U,  /* ERU OGU channel number */
};

__STATIC_INLINE void PIN_INTERRUPT_Enable(const PIN_INTERRUPT_t *const handle)
{
	NVIC_EnableIRQ(handle->IRQn);
}

__STATIC_INLINE void PIN_INTERRUPT_Disable(const PIN_INTERRUPT_t *const handle)
{
	NVIC_DisableIRQ(handle->IRQn);
}
__STATIC_INLINE uint32_t PIN_INTERRUPT_GetPinValue(const PIN_INTERRUPT_t *const handle)
{
	return (XMC_GPIO_GetInput(handle->port, handle->pin));
}

void PIN_INTERRUPT_Init(const PIN_INTERRUPT_t *const handle)
{
	/* Initializes input pin characteristics */
	XMC_GPIO_Init(handle->port, handle->pin, &handle->gpio_config);
	/* ERU Event Trigger Logic Hardware initialization based on UI */
	XMC_ERU_ETL_Init(handle->eru, handle->etl, &handle->etl_config);
	/* OGU is configured to generate event on configured trigger edge */
	XMC_ERU_OGU_SetServiceRequestMode(handle->eru, handle->ogu, XMC_ERU_OGU_SERVICE_REQUEST_ON_TRIGGER);
	/* Configure NVIC node and priority */
	NVIC_SetPriority((IRQn_Type)handle->IRQn, handle->irq_priority);
	/* Clear pending interrupt before enabling it */
	NVIC_ClearPendingIRQ((IRQn_Type)handle->IRQn);
	/* Enable NVIC node */
	NVIC_EnableIRQ((IRQn_Type)handle->IRQn);
}
void PIN_INTERRUPT_SetEdgeSensitivity(const PIN_INTERRUPT_t *const handle, const PIN_INTERRUPT_EDGE_t edge)
{
	XMC_ERU_ETL_SetEdgeDetection(handle->eru, handle->etl, (XMC_ERU_ETL_EDGE_DETECTION_t)edge);
}
PIN_INTERRUPT_EDGE_t PIN_INTERRUPT_GetEdgeSensitivity(const PIN_INTERRUPT_t *const handle)
{
	return ((PIN_INTERRUPT_EDGE_t)XMC_ERU_ETL_GetEdgeDetection(handle->eru, handle->etl));
}

