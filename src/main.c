#include "xmc_ccu4.h"
#include "xmc_scu.h"
#include "led_module.h"

const XMC_CCU4_SLICE_COMPARE_CONFIG_t RTE_slice_config =
{
  .timer_mode 		   = (uint32_t) XMC_CCU4_SLICE_TIMER_COUNT_MODE_EA,
  .monoshot   		   = (uint32_t) false,
  .shadow_xfer_clear   = (uint32_t) 0,
  .dither_timer_period = (uint32_t) 0,
  .dither_duty_cycle   = (uint32_t) 0,
  .prescaler_mode	   = (uint32_t) XMC_CCU4_SLICE_PRESCALER_MODE_NORMAL,
  .mcm_enable		   = (uint32_t) 0,
  .prescaler_initval   = (uint32_t) 6,
  .float_limit		   = (uint32_t) 0,
  .dither_limit		   = (uint32_t) 0,
  .passive_level 	   = (uint32_t) XMC_CCU4_SLICE_OUTPUT_PASSIVE_LEVEL_LOW,
  .timer_concatenation = (uint32_t) 0
};

volatile uint32_t tick5ms;

XMC_CCU4_MODULE_t * const base = (XMC_CCU4_MODULE_t*) CCU41_BASE;
XMC_CCU4_SLICE_t * const slice  = (XMC_CCU4_SLICE_t*) CCU41_CC41;
XMC_CCU4_MODULE_t * const ccu = (XMC_CCU4_MODULE_t*) CCU41;

int main(void)
{
  /* Enable CCU4 module */
  XMC_CCU4_Init(ccu, XMC_CCU4_SLICE_MCMS_ACTION_TRANSFER_PR_CR);
  /* Start the prescaler */
  XMC_CCU4_StartPrescaler(ccu);

  XMC_CCU4_SLICE_CompareInit(slice, &RTE_slice_config);

  /* Set the period and compare register values */
  XMC_CCU4_SLICE_SetTimerPeriodMatch(slice,
		                             (uint16_t)9375);
  /* Configuring for 5ms Interrupt, with prescaler 6 */

  XMC_CCU4_EnableShadowTransfer(base, (uint32_t)((uint32_t)XMC_CCU4_SHADOW_TRANSFER_SLICE_1 |
          (uint32_t)XMC_CCU4_SHADOW_TRANSFER_PRESCALER_SLICE_1));

  /* Initialize interrupts */
  XMC_CCU4_SLICE_EnableEvent(slice, XMC_CCU4_SLICE_IRQ_ID_PERIOD_MATCH);

  /* Bind event to Service Request Node to period match event*/
  XMC_CCU4_SLICE_SetInterruptNode(slice, XMC_CCU4_SLICE_IRQ_ID_PERIOD_MATCH,
		  XMC_CCU4_SLICE_SR_ID_1);
  XMC_CCU4_EnableClock(base , 1U); /* 1 stands for slice number */
  XMC_CCU4_SLICE_StartTimer(slice);

  NVIC_SetPriority(CCU41_1_IRQn,
                     NVIC_EncodePriority(NVIC_GetPriorityGrouping(),
                    		 	 	 	 63,
                                         0));
  NVIC_EnableIRQ(CCU41_1_IRQn);

  leds_init();


  uint32_t counter = 0;
  /**
   * @brief main() - Application entry point
   * No code in the main routine. All update made in the ISR handler.
   */
  while(1U)
  {
	  if(tick5ms != 0){
		  tick5ms--;
		  counter++;
		  if(counter >= 20){
			  counter = 0;
			  led_toggle(LED0);
		  }
	  }
  }
  return 1;
}

void CCU41_1_IRQHandler(void)
{
	XMC_CCU4_SLICE_ClearEvent(slice, XMC_CCU4_SLICE_IRQ_ID_PERIOD_MATCH);
	tick5ms++;
}
