/*
 * timetasks.c
 *
 *  Created on: Jul 11, 2017
 *      Author: chocolate
 */

#include "timetasks.h"
#include "xmc_ccu4.h"
#include "xmc_scu.h"
#include "led_module.h"

#define CU41_MODULE_PTR      	CCU41
#define MODULE_NUMBER   		(1U)

#define CU41_SL1_PTR       	CCU41_CC41
#define SLICE0_NUMBER    	(1U)

extern const XMC_CCU4_SLICE_COMPARE_CONFIG_t CU41_SLICE1_config;


void Tick_Interrupt_Init(void){
	XMC_CCU4_Init(CU41_MODULE_PTR, XMC_CCU4_SLICE_MCMS_ACTION_TRANSFER_PR_CR);
	XMC_CCU4_StartPrescaler(CU41_MODULE_PTR);

	XMC_CCU4_EnableClock(CCU41, SLICE0_NUMBER);
	XMC_CCU4_SLICE_CompareInit(CU41_SL1_PTR, &CU41_SLICE1_config);

	XMC_CCU4_SLICE_SetTimerPeriodMatch(CU41_SL1_PTR, 37499U);
	XMC_CCU4_SLICE_SetTimerCompareMatch(CU41_SL1_PTR, 0);  /* 0 -> 100% duty cycle */
	XMC_CCU4_EnableShadowTransfer(CU41_MODULE_PTR, 	\
			(uint32_t)(XMC_CCU4_SHADOW_TRANSFER_SLICE_0| XMC_CCU4_SHADOW_TRANSFER_PRESCALER_SLICE_0));


	XMC_CCU4_SLICE_SetInterruptNode(CU41_SL1_PTR, XMC_CCU4_SLICE_IRQ_ID_PERIOD_MATCH, \
			XMC_CCU4_SLICE_SR_ID_1);
	XMC_CCU4_SLICE_EnableEvent(CU41_SL1_PTR, XMC_CCU4_SLICE_IRQ_ID_PERIOD_MATCH);

	XMC_CCU4_SLICE_ClearTimer(CU41_SL1_PTR);
	XMC_CCU4_SLICE_StartTimer(CU41_SL1_PTR);

	NVIC_SetPriority(CCU41_1_IRQn,
			NVIC_EncodePriority(
					NVIC_GetPriorityGrouping(),
					63,
					0));
	NVIC_EnableIRQ(CCU41_1_IRQn);
}
