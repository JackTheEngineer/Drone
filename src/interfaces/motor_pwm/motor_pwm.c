/*
 * pwm.c
 *
 *  Created on: Apr 18, 2016
 *      Author: jakov
 */

#include "motor_pwm.h"
#include "xmc_scu.h"
#include "xmc_ccu8.h"

void SetCompareValue(local_pwm_t const * pwm, uint16_t compare_value);
void InitOnePWMPort(local_pwm_t const * pwm);

const XMC_CCU8_SLICE_COMPARE_CONFIG_t CCU8_timer_config     =
{
		.timer_mode            = (uint32_t)XMC_CCU8_SLICE_TIMER_COUNT_MODE_CA,
		.monoshot              = (uint32_t)XMC_CCU8_SLICE_TIMER_REPEAT_MODE_REPEAT,
		.shadow_xfer_clear     = 0U,
		.dither_timer_period   = 0U,
		.dither_duty_cycle     = 0U,

		.prescaler_mode        = (uint32_t)XMC_CCU8_SLICE_PRESCALER_MODE_NORMAL,
		.mcm_ch1_enable        = 0U,
		.mcm_ch2_enable        = 0U,

		.slice_status          = (uint32_t)XMC_CCU8_SLICE_STATUS_CHANNEL_1,

		.passive_level_out0    = (uint32_t)XMC_CCU8_SLICE_OUTPUT_PASSIVE_LEVEL_LOW,
		.passive_level_out1    = (uint32_t)XMC_CCU8_SLICE_OUTPUT_PASSIVE_LEVEL_LOW,
		.passive_level_out2    = (uint32_t)XMC_CCU8_SLICE_OUTPUT_PASSIVE_LEVEL_LOW,
		.passive_level_out3    = (uint32_t)XMC_CCU8_SLICE_OUTPUT_PASSIVE_LEVEL_LOW,

		.asymmetric_pwm        = 0U,
#if !defined(CCU8V3)
		.invert_out0           = 1U,
		.invert_out1           = 1U,
		.invert_out2           = 1U,
		.invert_out3           = 1U,
#else
		.selector_out0         = XMC_CCU8_SOURCE_OUT0_ST1,
		.selector_out1         = XMC_CCU8_SOURCE_OUT1_INV_ST1,
		.selector_out2         = XMC_CCU8_SOURCE_OUT2_ST2,
		.selector_out3         = XMC_CCU8_SOURCE_OUT3_INV_ST2,
#endif
		.prescaler_initval     = 4U,
		.float_limit           = 0U,
		.dither_limit          = 0U,
		.timer_concatenation   = 0U,
};

const XMC_GPIO_CONFIG_t  motor_pwm_out_config   =
{
		.mode                = XMC_GPIO_MODE_OUTPUT_PUSH_PULL_ALT3,
		.output_level        = XMC_GPIO_OUTPUT_LEVEL_LOW,
		.output_strength     = XMC_GPIO_OUTPUT_STRENGTH_WEAK,
};

typedef struct _Easy_CCU8_PWM_{
	XMC_CCU8_MODULE_t * const module; /* CCU8 Base pointer, (XMC_CCU8_MODULE_t*) CCU80; */
	XMC_CCU8_SLICE_t * const slice;	  /* slice  base pointer, f.e (XMC_CCU8_SLICE_t*) CCU80_CC80; */
	uint8_t slice_number;
	XMC_CCU8_SLICE_COMPARE_CHANNEL_t compare_channel;
	uint32_t  shadow_transfer_enable_code; /* f.e XMC_CCU8_SHADOW_TRANSFER_SLICE_0; */
	uint32_t  shadow_transfer_enable_dither_code; /* f.e XMC_CCU8_SHADOW_TRANSFER_DITHER_SLICE_0; */
	uint16_t period_match_value; 
	XMC_GPIO_PORT_t * gpio_base; /* f.e. (XMC_GPIO_PORT_t *) PORT0_BASE; */
	uint32_t gpio_pin; /* pin number */
	XMC_GPIO_CONFIG_t const * gpio_config_ptr; 
}local_pwm_t;


/* For the appropriate available pinouts lookup 
 * page 2557 of the micro-Keil xmc4500 manual 
 */
const local_pwm_t pwm1_container = {
	.module = (XMC_CCU8_MODULE_t*) CCU80,
	.slice = (XMC_CCU8_SLICE_t*) CCU80_CC80,
	.slice_number = 0U,
	.compare_channel =  XMC_CCU8_SLICE_COMPARE_CHANNEL_1,
	.shadow_transfer_enable_code = XMC_CCU8_SHADOW_TRANSFER_SLICE_0,
	.shadow_transfer_enable_dither_code = XMC_CCU8_SHADOW_TRANSFER_DITHER_SLICE_0,
	.period_match_value = 1000U,
	.gpio_base  = (XMC_GPIO_PORT_t *) PORT0_BASE,
	.gpio_pin = 2U,
	.gpio_config_ptr = &motor_pwm_out_config,
};
const local_pwm_t pwm2_container = {
	.module = (XMC_CCU8_MODULE_t*) CCU80,
	.slice = (XMC_CCU8_SLICE_t*) CCU80_CC82,
	.slice_number = 2U,
	.compare_channel =  XMC_CCU8_SLICE_COMPARE_CHANNEL_1,
	.shadow_transfer_enable_code = XMC_CCU8_SHADOW_TRANSFER_SLICE_2,
	.shadow_transfer_enable_dither_code = XMC_CCU8_SHADOW_TRANSFER_DITHER_SLICE_2,
	.period_match_value = 1000U,
	.gpio_base  = (XMC_GPIO_PORT_t *) PORT0_BASE,
	.gpio_pin = 3U,
	.gpio_config_ptr = &motor_pwm_out_config,
};
const local_pwm_t pwm3_container = {
	.module = (XMC_CCU8_MODULE_t*) CCU80,
	.slice = (XMC_CCU8_SLICE_t*) CCU80_CC83,
	.slice_number = 3U,
	.compare_channel =  XMC_CCU8_SLICE_COMPARE_CHANNEL_1,
	.shadow_transfer_enable_code = XMC_CCU8_SHADOW_TRANSFER_SLICE_3,
	.shadow_transfer_enable_dither_code = XMC_CCU8_SHADOW_TRANSFER_DITHER_SLICE_3,
	.period_match_value = 1000U,
	.gpio_base  = (XMC_GPIO_PORT_t *) PORT0_BASE,
	.gpio_pin = 6U,
	.gpio_config_ptr = &motor_pwm_out_config,
};
const local_pwm_t pwm4_container = {
	.module = (XMC_CCU8_MODULE_t*) CCU80,
	.slice = (XMC_CCU8_SLICE_t*) CCU80_CC81,
	.slice_number = 1U,
	.compare_channel =  XMC_CCU8_SLICE_COMPARE_CHANNEL_1,
	.shadow_transfer_enable_code = XMC_CCU8_SHADOW_TRANSFER_SLICE_1,
	.shadow_transfer_enable_dither_code = XMC_CCU8_SHADOW_TRANSFER_DITHER_SLICE_1,
	.period_match_value = 1000U,
	.gpio_base  = (XMC_GPIO_PORT_t *) PORT0_BASE,
	.gpio_pin = 4U,
	.gpio_config_ptr = &motor_pwm_out_config,
};

local_pwm_t const *pwm1 = &pwm1_container;
local_pwm_t const *pwm2 = &pwm2_container;
local_pwm_t const *pwm3 = &pwm3_container;
local_pwm_t const *pwm4 = &pwm4_container;

void SetCompareValue(local_pwm_t const * pwm, uint16_t compare_value){
	XMC_CCU8_SLICE_SetTimerCompareMatch(pwm->slice,
					    pwm->compare_channel,
					    compare_value);
	XMC_CCU8_EnableShadowTransfer(pwm->module,
				      pwm->shadow_transfer_enable_code);
	XMC_CCU8_EnableShadowTransfer(pwm->module,
				      pwm->shadow_transfer_enable_dither_code);
}

/* Requires value from 0 - 1000 */
void PWM_Motor1_Set_Rate(uint16_t Speed){//
	SetCompareValue(pwm1, Speed);
}
void PWM_Motor2_Set_Rate(uint16_t Speed){
	SetCompareValue(pwm2, Speed);
}
void PWM_Motor3_Set_Rate(uint16_t Speed){
	SetCompareValue(pwm3, Speed);
}
void PWM_Motor4_Set_Rate(uint16_t Speed){
	SetCompareValue(pwm4, Speed);
}

void InitOnePWMPort(local_pwm_t const * pwm){
	/* Initialize consumed Apps */
	XMC_CCU8_Init(pwm->module, XMC_CCU8_SLICE_MCMS_ACTION_TRANSFER_PR_CR);

	/* Start the prescaler */
	XMC_CCU8_StartPrescaler(pwm->module);
	XMC_CCU8_SLICE_CompareInit(pwm->slice, &CCU8_timer_config);
	
	/* Set period match value of the timer  */
	XMC_CCU8_SLICE_SetTimerPeriodMatch(pwm->slice, pwm->period_match_value);

	/* Set timer compare match value for channel 1 */
	/* Set the pwm compare to 0, so there is no output */
	SetCompareValue(pwm, 0);

	XMC_GPIO_Init(pwm->gpio_base,
		      pwm->gpio_pin,
		      pwm->gpio_config_ptr);
	
	XMC_CCU8_EnableClock(pwm->module, pwm->slice_number);
	XMC_CCU8_SLICE_StartTimer(pwm->slice);
}
	
void PWM_Init(void){
	InitOnePWMPort(pwm1);
	InitOnePWMPort(pwm2);
	InitOnePWMPort(pwm3);
	InitOnePWMPort(pwm4);
}





