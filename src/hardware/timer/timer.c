/*
 * tick_.c
 *
 *  Created on: Jul 20, 2017
 *      Author: chocolate
 */
#include "timer.h"
#include "xmc_ccu4.h"
#include "xmc_scu.h"

XMC_CCU4_MODULE_t * const BASE = (XMC_CCU4_MODULE_t*) CCU41_BASE;
XMC_CCU4_SLICE_t * const SLICE  = (XMC_CCU4_SLICE_t*) CCU41_CC41;
XMC_CCU4_MODULE_t * const CCU = (XMC_CCU4_MODULE_t*) CCU41;

const XMC_CCU4_SLICE_COMPARE_CONFIG_t RTE_slice_config =
{
  .timer_mode 		   = (uint32_t) XMC_CCU4_SLICE_TIMER_COUNT_MODE_EA,
  .monoshot   		   = (uint32_t) false,
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

void TIMER_Init(void){
	 /* Enable CCU4 module */
	  XMC_CCU4_Init(CCU, XMC_CCU4_SLICE_MCMS_ACTION_TRANSFER_PR_CR);
	  /* Start the prescaler */
	  XMC_CCU4_StartPrescaler(CCU);
	  XMC_CCU4_SLICE_CompareInit(SLICE, &RTE_slice_config);
	  /* Set the period and compare register values */
	  XMC_CCU4_SLICE_SetTimerPeriodMatch(SLICE,
			                             (uint16_t)30);
	  /* Approximately a 20 us */
	  XMC_CCU4_EnableShadowTransfer(BASE, (uint32_t)(
			  (uint32_t)XMC_CCU4_SHADOW_TRANSFER_SLICE_1 |
	          (uint32_t)XMC_CCU4_SHADOW_TRANSFER_PRESCALER_SLICE_1));

	  /* Initialize interrupts */
	  XMC_CCU4_SLICE_EnableEvent(SLICE, XMC_CCU4_SLICE_IRQ_ID_PERIOD_MATCH);
	  /* Bind event to Service Request Node to period match event*/
	  XMC_CCU4_SLICE_SetInterruptNode(SLICE,
			  XMC_CCU4_SLICE_IRQ_ID_PERIOD_MATCH,
			  XMC_CCU4_SLICE_SR_ID_1);
	  XMC_CCU4_EnableClock(BASE , 1U); /* 1 stands for slice number */

	  NVIC_SetPriority(CCU41_1_IRQn,
	                     NVIC_EncodePriority(NVIC_GetPriorityGrouping(),
	                    		 	 	 	 63, /* Level 0 is the highest level */
	                                         0));
	  NVIC_EnableIRQ(CCU41_1_IRQn);
}

void TIMER_Start(void){
	XMC_CCU4_SLICE_StartTimer(SLICE);
}

void TIMER_Stop(void){
	XMC_CCU4_SLICE_StopTimer(SLICE);
}

void TIMER_ClearEvent(void){
	XMC_CCU4_SLICE_ClearEvent(SLICE, XMC_CCU4_SLICE_IRQ_ID_PERIOD_MATCH);
}

void TIMER_Clear(void)
{
	XMC_CCU4_SLICE_ClearTimer(SLICE);
}
