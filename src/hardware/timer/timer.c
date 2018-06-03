/*
 * tick_.c
 *
 *  Created on: Jul 20, 2017
 *      Author: chocolate
 */
#include "timer.h"


const XMC_CCU4_SLICE_COMPARE_CONFIG_t PinPulse_slice_config =
{
		.timer_mode 		   = (uint32_t) XMC_CCU4_SLICE_TIMER_COUNT_MODE_EA,
		.monoshot   		   = XMC_CCU4_SLICE_TIMER_REPEAT_MODE_REPEAT,
		.shadow_xfer_clear   = (uint32_t) 0,
		.dither_timer_period = (uint32_t) 0,
		.dither_duty_cycle   = (uint32_t) 0,
		.prescaler_mode	   = (uint32_t) XMC_CCU4_SLICE_PRESCALER_MODE_NORMAL,
		.mcm_enable		   = (uint32_t) 0,
		.prescaler_initval   = (uint32_t) 7,
		.float_limit		   = (uint32_t) 0,
		.dither_limit		   = (uint32_t) 0,
		.passive_level 	   = (uint32_t) XMC_CCU4_SLICE_OUTPUT_PASSIVE_LEVEL_LOW,
		.timer_concatenation = (uint32_t) 0
};

void TIMER_Init_with_params(XMC_CCU4_MODULE_t * const module,
		XMC_CCU4_SLICE_t * const slice,
		XMC_CCU4_SLICE_COMPARE_CONFIG_t * const timer_config,
		uint8_t slice_number)
{

	XMC_CCU4_Init(module, XMC_CCU4_SLICE_MCMS_ACTION_TRANSFER_PR_CR);
	XMC_CCU4_StartPrescaler(module);
	XMC_CCU4_EnableClock(module , slice_number);

	XMC_CCU4_SLICE_CompareInit(slice, timer_config);
	XMC_CCU4_SLICE_SetTimerPeriodMatch(slice, (uint16_t)30);
	XMC_CCU4_EnableShadowTransfer(module, (uint32_t)(
					(uint32_t)XMC_CCU4_SHADOW_TRANSFER_SLICE_1 |
					(uint32_t)XMC_CCU4_SHADOW_TRANSFER_PRESCALER_SLICE_1));

	XMC_CCU4_SLICE_SetInterruptNode(slice, XMC_CCU4_SLICE_IRQ_ID_PERIOD_MATCH,
			(XMC_CCU4_SLICE_SR_ID_t)slice_number);
	XMC_CCU4_SLICE_EnableEvent(slice, XMC_CCU4_SLICE_IRQ_ID_PERIOD_MATCH);
	XMC_CCU4_SLICE_ClearTimer(slice);

}

void TIMER_Start(XMC_CCU4_SLICE_t * const slice)
{
	XMC_CCU4_SLICE_StartTimer(slice);
}

void TIMER_Stop(XMC_CCU4_SLICE_t * const slice)
{
	XMC_CCU4_SLICE_StopTimer(slice);
}

void TIMER_ClearEvent(XMC_CCU4_SLICE_t * const slice)
{
	XMC_CCU4_SLICE_ClearEvent(slice,
			XMC_CCU4_SLICE_IRQ_ID_PERIOD_MATCH);
}

void TIMER_Clear(XMC_CCU4_SLICE_t * const slice)
{
	XMC_CCU4_SLICE_ClearTimer(slice);
}
