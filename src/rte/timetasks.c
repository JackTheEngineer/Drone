/*
 * timetasks.c
 *
 *  Created on: Jul 11, 2017
 *      Author: chocolate
 */

#include "timetasks.h"
#include "xmc4_scu.h"

#define CU41_MODULE_PTR      	CCU41
#define MODULE_NUMBER   		(0U)

#define SLICE0_PTR       	CCU41_CC40
#define SLICE0_NUMBER    	(0U)

extern XMC_CCU4_SLICE_EVENT_CONFIG_t CU41_SLICE0_event0_config;
extern uint32_t GlobalTimerSignal;

void CCU40_0_IRQHandler(void);

void Run_Next_Task(uint32_t *ticks){
	if(*ticks % (MS_100)){
		led_toggle(LED1);
		led_toggle(LED0);
	}
}

void Tick_Interrupt_Init(void){
	XMC_CCU4_Init(CU41_MODULE_PTR, XMC_CCU4_SLICE_MCMS_ACTION_TRANSFER_PR_CR);
	XMC_CCU4_StartPrescaler(CU41_MODULE_PTR);
	XMC_CCU4_SetModuleClock(CU41_MODULE_PTR, XMC_CCU4_CLOCK_SCU);
	XMC_CCU4_SLICE_CompareInit(SLICE0_PTR, &SLICE0_config);
	XMC_CCU4_SLICE_SetTimerCompareMatch(SLICE0_PTR, 10000);
	XMC_CCU4_SLICE_SetTimerPeriodMatch(SLICE0_PTR, 62499U);
	XMC_CCU4_EnableShadowTransfer(CU41_MODULE_PTR, 	\
			(uint32_t)(XMC_CCU4_SHADOW_TRANSFER_SLICE_0|	\
					XMC_CCU4_SHADOW_TRANSFER_PRESCALER_SLICE_0));
	XMC_CCU4_SLICE_ConfigureEvent(SLICE0_PTR, XMC_CCU4_SLICE_EVENT_0, &CU41_SLICE0_event0_config);
	XMC_CCU4_SLICE_StartConfig(SLICE0_PTR, XMC_CCU4_SLICE_EVENT_0, \
	XMC_CCU4_SLICE_START_MODE_TIMER_START_CLEAR);
	XMC_CCU4_SLICE_EnableEvent(SLICE0_PTR, XMC_CCU4_SLICE_IRQ_ID_COMPARE_MATCH_UP);
	XMC_CCU4_SLICE_SetInterruptNode(SLICE0_PTR, \
			XMC_CCU4_SLICE_IRQ_ID_COMPARE_MATCH_UP, XMC_CCU4_SLICE_SR_ID_0);
	NVIC_SetPriority(CCU41_0_IRQn, 3U);
	NVIC_EnableIRQ(CCU41_0_IRQn);
	XMC_CCU4_EnableClock(CU41_MODULE_PTR, SLICE0_NUMBER);
	XMC_SCU_SetCcuTriggerHigh(XMC_SCU_CCU_TRIGGER_CCU40);

}
/* Tick_Interrupt */
void CCU40_0_IRQHandler(void)
{
	/* Clear pending interrupt */
	XMC_CCU4_SLICE_ClearEvent(SLICE0_PTR, XMC_CCU4_SLICE_IRQ_ID_COMPARE_MATCH_UP);
	GlobalTimerSignal++;
}
