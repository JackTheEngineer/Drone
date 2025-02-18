#ifndef TIMER_H
#define TIMER_H

#include "xmc_ccu4.h"
#include "xmc_scu.h"

#define DelayUsHandler CCU40_1_IRQHandler
#define PinPulseHandler CCU40_0_IRQHandler

extern const XMC_CCU4_SLICE_COMPARE_CONFIG_t TIMER_0_config;

void Handwritten_TIMER_Init_with_params(XMC_CCU4_MODULE_t * const module,
		XMC_CCU4_SLICE_t * const slice,
		XMC_CCU4_SLICE_COMPARE_CONFIG_t * const timer_config,
		uint8_t slice_number);
void Handwritten_TIMER_Start(XMC_CCU4_SLICE_t * const slice);
void Handwritten_TIMER_Stop(XMC_CCU4_SLICE_t * const slice);
void Handwritten_TIMER_ClearEvent(XMC_CCU4_SLICE_t * const slice);
void Handwritten_TIMER_Clear(XMC_CCU4_SLICE_t * const slice);

#endif /* TIMER_H */
