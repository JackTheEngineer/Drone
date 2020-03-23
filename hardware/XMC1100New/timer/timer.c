#include "timer.h"

const XMC_CCU4_SLICE_COMPARE_CONFIG_t TIMER_0_config =
{
		.timer_mode          = XMC_CCU4_SLICE_TIMER_COUNT_MODE_EA,
		.monoshot            = XMC_CCU4_SLICE_TIMER_REPEAT_MODE_REPEAT,
		.shadow_xfer_clear   = false,
		.dither_timer_period = false,
		.dither_duty_cycle   = false,
		.prescaler_mode      = XMC_CCU4_SLICE_PRESCALER_MODE_NORMAL,
		.mcm_enable          = false,
		.prescaler_initval   = 6U,
		.float_limit         = 0U,
		.dither_limit        = 0U,
		.passive_level       = XMC_CCU4_SLICE_OUTPUT_PASSIVE_LEVEL_LOW,
		.timer_concatenation = false
};

void Handwritten_TIMER_Init_with_params(XMC_CCU4_MODULE_t * const module,
		XMC_CCU4_SLICE_t * const slice,
		XMC_CCU4_SLICE_COMPARE_CONFIG_t * const timer_config,
		uint8_t slice_number)
{

	XMC_CCU4_Init(module, XMC_CCU4_SLICE_MCMS_ACTION_TRANSFER_PR_CR);
	XMC_CCU4_StartPrescaler(module);
	XMC_CCU4_EnableClock(module, slice_number);

	XMC_CCU4_SLICE_CompareInit(slice, timer_config);
	XMC_CCU4_SLICE_SetTimerPeriodMatch(slice, (uint16_t)30);
	XMC_CCU4_SLICE_SetTimerCompareMatch(slice, (uint16_t)0);
	XMC_CCU4_EnableShadowTransfer(module, (uint32_t)((uint32_t)XMC_CCU4_SHADOW_TRANSFER_SLICE_0 |
			(uint32_t)XMC_CCU4_SHADOW_TRANSFER_PRESCALER_SLICE_0));

	XMC_CCU4_SLICE_SetInterruptNode(slice, XMC_CCU4_SLICE_IRQ_ID_PERIOD_MATCH,
			(XMC_CCU4_SLICE_SR_ID_t)slice_number);
	XMC_CCU4_SLICE_EnableEvent(slice, XMC_CCU4_SLICE_IRQ_ID_PERIOD_MATCH);
	XMC_CCU4_SLICE_ClearTimer(slice);
}

void Handwritten_TIMER_Start(XMC_CCU4_SLICE_t * const slice)
{
	XMC_CCU4_SLICE_StartTimer(slice);
}

void Handwritten_TIMER_Stop(XMC_CCU4_SLICE_t * const slice)
{
	XMC_CCU4_SLICE_StopTimer(slice);
}

void Handwritten_TIMER_ClearEvent(XMC_CCU4_SLICE_t * const slice)
{
	XMC_CCU4_SLICE_ClearEvent(slice,
			XMC_CCU4_SLICE_IRQ_ID_PERIOD_MATCH);
}

void Handwritten_TIMER_Clear(XMC_CCU4_SLICE_t * const slice)
{
	XMC_CCU4_SLICE_ClearTimer(slice);
}
