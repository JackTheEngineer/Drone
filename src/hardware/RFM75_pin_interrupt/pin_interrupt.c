#include "pin_interrupt.h"

const PIN_INTERRUPT_t RFM75_PIN_INTERRUPT =
{
	.eru = XMC_ERU0,  /* ERU module 0 Mapped */
	.port = XMC_GPIO_PORT3,  /* PORT 3 Mapped */
	.pin = 2U, /* Mapped pin number */
	.gpio_config = {
		.mode = XMC_GPIO_MODE_INPUT_TRISTATE  /* Pin Characteristics */
	},
	.etl_config = {
		.input_a = (uint32_t)XMC_ERU_ETL_INPUT_A1, /* Event input selection for A(0-3) */
		.input_b = (uint32_t)XMC_ERU_ETL_INPUT_B0, /* Event input selection for B(0-3) */
		.enable_output_trigger = (uint32_t)1,
		/* Select the edge to convert as event */
		.edge_detection = (XMC_ERU_ETL_EDGE_DETECTION_t)PIN_INTERRUPT_EDGE_FALLING,
		/* Select the source for event */
		.output_trigger_channel = XMC_ERU_ETL_OUTPUT_TRIGGER_CHANNEL3, 
		.source = XMC_ERU_ETL_SOURCE_A
	},
	.IRQn = (IRQn_Type)4,  /* Mapped Interrupt Request Number */
	.irq_priority = 3, /* Priority of the Interrupt */
	.irq_subpriority = 0,  /* Subpriority of the Interrupt */
	.etl = 0U,  /* ERU ETL channel number */
	.ogu = 3U,  /* ERU OGU channel number */
};

void PIN_INTERRUPT_SetEdgeSensitivity(const PIN_INTERRUPT_t *const handle, const PIN_INTERRUPT_EDGE_t edge);
PIN_INTERRUPT_EDGE_t PIN_INTERRUPT_GetEdgeSensitivity(const PIN_INTERRUPT_t *const handle);

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
	XMC_GPIO_Init(handle->port, handle->pin, &handle->gpio_config);
	XMC_ERU_ETL_Init(handle->eru, handle->etl, &handle->etl_config);
	/* OGU = output gating unit */
	XMC_ERU_OGU_SetServiceRequestMode(handle->eru, handle->ogu, XMC_ERU_OGU_SERVICE_REQUEST_ON_TRIGGER);
	NVIC_SetPriority((IRQn_Type)handle->IRQn, NVIC_EncodePriority(NVIC_GetPriorityGrouping(),
								      handle->irq_priority, handle->irq_subpriority));
	NVIC_ClearPendingIRQ((IRQn_Type)handle->IRQn);
	NVIC_EnableIRQ((IRQn_Type)handle->IRQn);
}

void PIN_INTERRUPT_SetEdgeSensitivity(const PIN_INTERRUPT_t *const handle, const PIN_INTERRUPT_EDGE_t edge)
{
	XMC_ERU_ETL_SetEdgeDetection(handle->eru, handle->etl,
				     (XMC_ERU_ETL_EDGE_DETECTION_t)edge);
}

PIN_INTERRUPT_EDGE_t PIN_INTERRUPT_GetEdgeSensitivity(const PIN_INTERRUPT_t *const handle)
{
	return ((PIN_INTERRUPT_EDGE_t)XMC_ERU_ETL_GetEdgeDetection(handle->eru, handle->etl));
}
