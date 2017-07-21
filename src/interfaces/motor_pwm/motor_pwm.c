/*
 * pwm.c
 *
 *  Created on: Apr 18, 2016
 *      Author: jakov
 */

#include "motor_pwm.h"
#include "xmc_scu.h"
#include "xmc_ccu8.h"

#include "xmc_common.h"
#include "clock_xmc4.h"
#include "global_ccu8.h"
#include "pwm_ccu8.h"

#include "pwm_ccu8.h"
#include "global_ccu8.h"

/**< Configuration for HandleGLOBAL_CCU8_0 */
GLOBAL_CCU8_t GLOBAL_CCU8_0 =
{
  .module_frequency = 120000000U,  /**< CCU8 input clock frequency */
  .syncstart_trigger_msk = XMC_SCU_CCU_TRIGGER_CCU80,
  .module_ptr = (XMC_CCU8_MODULE_t*) CCU80,      /**< CCU8 Module Pointer */
  .mcs_action = (XMC_CCU8_SLICE_MCMS_ACTION_t)XMC_CCU8_SLICE_MCMS_ACTION_TRANSFER_PR_CR,
};


const XMC_CCU8_SLICE_COMPARE_CONFIG_t  PWM_CCU8_0_timer_handle     =
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
		.invert_out0           = 0U,
		.invert_out1           = 1U,
		.invert_out2           = 0U,
		.invert_out3           = 0U,
#else
		.selector_out0         = XMC_CCU8_SOURCE_OUT0_ST1,
		.selector_out1         = XMC_CCU8_SOURCE_OUT1_INV_ST1,
		.selector_out2         = XMC_CCU8_SOURCE_OUT2_ST2,
		.selector_out3         = XMC_CCU8_SOURCE_OUT3_INV_ST2,
#endif
		.prescaler_initval     = 3U,
		.float_limit           = 0U,
		.dither_limit          = 0U,
		.timer_concatenation   = 0U,
};


const XMC_CCU8_SLICE_EVENT_CONFIG_t PWM_inactive_event =
{
		.mapped_input        = XMC_CCU8_SLICE_INPUT_H,
		.edge                = XMC_CCU8_SLICE_EVENT_EDGE_SENSITIVITY_NONE,
		.level               = XMC_CCU8_SLICE_EVENT_LEVEL_SENSITIVITY_ACTIVE_LOW,
		.duration            = XMC_CCU8_SLICE_EVENT_FILTER_DISABLED,
};

const XMC_CCU8_SLICE_DEAD_TIME_CONFIG_t  PWM_CCU8_0_dt_config =
{
		.enable_dead_time_channel1         = 0U,
		.enable_dead_time_channel2         = 0U,
		.channel1_st_path                  = 0U,
		.channel1_inv_st_path              = 0U,
		.channel2_st_path                  = 0U,
		.channel2_inv_st_path              = 0U,
		.div                               = (uint32_t)XMC_CCU8_SLICE_DTC_DIV_1,

		.channel1_st_rising_edge_counter   = 0U,
		.channel1_st_falling_edge_counter  = 0U,

		.channel2_st_rising_edge_counter   = 0U,
		.channel2_st_falling_edge_counter  = 0U,
};

const XMC_GPIO_CONFIG_t  motor_pwm_out_config   =
{
		.mode                = XMC_GPIO_MODE_OUTPUT_PUSH_PULL_ALT3,
		.output_level        = XMC_GPIO_OUTPUT_LEVEL_LOW,
		.output_strength     = XMC_GPIO_OUTPUT_STRENGTH_WEAK,
};

const PWM_CCU8_CONFIG_t  PWM_CCU8_0_config_handle =
{
		.start_control                       = true,
		.period_value                        = 800U,
		.compare1_value                      = 100U,
		.compare2_value                      = 200U,

		.int_per_match                       = false,
		.int_cmp1_match_up                   = false,
		.int_cmp1_match_down                 = false,
		.int_cmp2_match_up                   = false,
		.int_cmp2_match_down                 = false,
		.int_one_match_down                  = false,
		.int_e0                              = false,
		.int_e1                              = false,
		.int_e2                              = false,

		.sr_per_match                        = XMC_CCU8_SLICE_SR_ID_0,
		.sr_cmp1_match_up                    = XMC_CCU8_SLICE_SR_ID_0,
		.sr_cmp1_match_down                  = XMC_CCU8_SLICE_SR_ID_0,
		.sr_cmp2_match_up                    = XMC_CCU8_SLICE_SR_ID_0,
		.sr_cmp2_match_down                  = XMC_CCU8_SLICE_SR_ID_0,
		.sr_one_match_down                   = XMC_CCU8_SLICE_SR_ID_0,
		.sr_e0                               = XMC_CCU8_SLICE_SR_ID_0,
		.sr_e1                               = XMC_CCU8_SLICE_SR_ID_0,
		.sr_e2                               = XMC_CCU8_SLICE_SR_ID_0,

		.event0_config_ptr                   = &PWM_inactive_event,
		.event1_config_ptr                   = &PWM_inactive_event,
		.event2_config_ptr                   = &PWM_inactive_event,

		.ext_start_event                     = XMC_CCU8_SLICE_EVENT_NONE,
		.ext_start_mode                      = XMC_CCU8_SLICE_START_MODE_TIMER_START,

		.ext_stop_event                      = XMC_CCU8_SLICE_EVENT_NONE,
		.ext_stop_mode                       = XMC_CCU8_SLICE_END_MODE_TIMER_STOP,

		.ext_count_dir_event                 = XMC_CCU8_SLICE_EVENT_NONE,

		.ext_gate_event                      = XMC_CCU8_SLICE_EVENT_NONE,

		.ext_count_event                     = XMC_CCU8_SLICE_EVENT_NONE,

		.ext_load_event                      = XMC_CCU8_SLICE_EVENT_NONE,
		.ext_load_selector                   = XMC_CCU8_SLICE_COMPARE_CHANNEL_1,

		.ext_mod_event                       = XMC_CCU8_SLICE_EVENT_NONE,
		.ext_mod_mode                        = XMC_CCU8_SLICE_MODULATION_MODE_CLEAR_ST_OUT,
		.ext_mod_sync                        = false,

		.ext_override_edge_event             = XMC_CCU8_SLICE_EVENT_NONE,

		.ext_override_level_event            = XMC_CCU8_SLICE_EVENT_NONE,

		.ext_trap_enable                     = false,
		.ext_trap_event                      = XMC_CCU8_SLICE_EVENT_NONE,
		.ext_trap_sync                       = false,
		.ext_trap_exit                       = XMC_CCU8_SLICE_TRAP_EXIT_MODE_AUTOMATIC,

		.mcm_shadow_txfr_mode                = XMC_CCU8_MULTI_CHANNEL_SHADOW_TRANSFER_SW_SLICE0,

		.dt_config_ptr                       = &PWM_CCU8_0_dt_config,

#if (UC_SERIES != XMC45) /*STC register not available on XMC45xx devices */
		.shadow_transfer_mode                = XMC_CCU8_SLICE_SHADOW_TRANSFER_MODE_ONLY_IN_PERIOD_MATCH,
#endif

#if (UC_SERIES == XMC14) /*below feature available in XMC14xx devices */
		.immediate_write                     = 0U,

		.automatic_shadow_transfer           = 0U,

		.cascaded_shadow_txfr_enable         = false,
#endif

		.ccu8_cc8_slice_timer_ptr            = &PWM_CCU8_0_timer_handle,

		.gpio_ch1_out0_enable                = false,
		.gpio_ch1_out0_ptr                   = (XMC_GPIO_PORT_t *) NULL,
		.gpio_ch1_out0_pin                   = 0U,
		.gpio_ch1_out0_config_ptr            = NULL,

		.gpio_ch1_out1_enable                = true,
		.gpio_ch1_out1_ptr                   = (XMC_GPIO_PORT_t *) PORT0_BASE,
		.gpio_ch1_out1_pin                   = 2U,
		.gpio_ch1_out1_config_ptr            = &motor_pwm_out_config,

		.gpio_ch2_out2_enable                = false,
		.gpio_ch2_out2_ptr                   = (XMC_GPIO_PORT_t *) NULL,
		.gpio_ch2_out2_pin                   = 0U,
		.gpio_ch2_out2_config_ptr            = NULL,

		.gpio_ch2_out3_enable                = false,
		.gpio_ch2_out3_ptr                   = (XMC_GPIO_PORT_t *) NULL,
		.gpio_ch2_out3_pin                   = 0U,
		.gpio_ch2_out3_config_ptr            = NULL,

		.global_ccu8_handle                   = (GLOBAL_CCU8_t*) &GLOBAL_CCU8_0,
};

PWM_CCU8_t PWM_CCU8_0 =
{
		.config_ptr                          = &PWM_CCU8_0_config_handle,
		.ccu8_module_ptr                     = (XMC_CCU8_MODULE_t*) CCU80_BASE,
		.ccu8_slice_ptr                      = (XMC_CCU8_SLICE_t*) CCU80_CC80,
		.kernel_number                       = 0U,
		.slice_number                        = 0U,
		.shadow_txfr_msk                     = (uint32_t)XMC_CCU8_SHADOW_TRANSFER_SLICE_0,
		.dither_shadow_txfr_msk              = (uint32_t)XMC_CCU8_SHADOW_TRANSFER_DITHER_SLICE_0,
		.prescaler_shadow_txfr_msk           = (uint32_t)XMC_CCU8_SHADOW_TRANSFER_PRESCALER_SLICE_0,

		.sym_duty1                           = 0U,
		.sym_duty2                           = 0U,
		.asym_duty                           = 0U,
		.asym_shift                          = 0U,
};

static void PWM_CCU8_lInit_Interrupt(PWM_CCU8_t* handle_ptr);
static void PWM_CCU8_lConfigure_Events(PWM_CCU8_t* handle_ptr);
static void PWM_CCU8_lInit_GPIO(PWM_CCU8_t* handle_ptr);

PWM_CCU8_t * const handle_ptr = &PWM_CCU8_0;

/* Requires value from 0 - 1000 */
void PWM_Motor1_Set_Rate(uint16_t Speed){//
	XMC_CCU8_SLICE_SetTimerCompareMatch((XMC_CCU8_SLICE_t*) CCU80_CC80, XMC_CCU8_SLICE_COMPARE_CHANNEL_1,
			(uint16_t) Speed);
	XMC_CCU8_EnableShadowTransfer(handle_ptr->ccu8_module_ptr, handle_ptr->shadow_txfr_msk);
	XMC_CCU8_EnableShadowTransfer(handle_ptr->ccu8_module_ptr, handle_ptr->dither_shadow_txfr_msk);
}

void PWM_Motor2_Set_Rate(uint16_t Speed){
}
void PWM_Motor3_Set_Rate(uint16_t Speed){

}
void PWM_Motor4_Set_Rate(uint16_t Speed){

}

void PWM_Init(void){
	PWM_CCU8_t * const handle_ptr = &PWM_CCU8_0;


	uint32_t frequency_module;
	uint32_t prescalar;

	XMC_CCU8_MODULE_t * const module = (XMC_CCU8_MODULE_t*) CCU80;
	XMC_CCU8_SLICE_t * const slice = (XMC_CCU8_SLICE_t*) CCU80_CC80;
	uint8_t const slice_number = 0U;
	uint32_t const shadow_transfer_enable_code = XMC_CCU8_SHADOW_TRANSFER_SLICE_0;
	uint32_t const shadow_transfer_enable_dither_code = XMC_CCU8_SHADOW_TRANSFER_DITHER_SLICE_0;

	/* Initialize consumed Apps */
	XMC_CCU8_Init(module,
		      XMC_CCU8_SLICE_MCMS_ACTION_TRANSFER_PR_CR);

	/* Start the prescaler */
	XMC_CCU8_StartPrescaler(module);
	/* Restricts multiple initializations */

	XMC_CCU8_SLICE_CompareInit(slice, &PWM_CCU8_0_timer_handle);
	
	/* Set period match value of the timer  */
	XMC_CCU8_SLICE_SetTimerPeriodMatch(slice, 800U);

	/* Set timer compare match value for channel 1 */
	XMC_CCU8_SLICE_SetTimerCompareMatch(slice, XMC_CCU8_SLICE_COMPARE_CHANNEL_1, (uint16_t) 0);
	/* Set timer compare match value for channel 2 */
	XMC_CCU8_SLICE_SetTimerCompareMatch(slice, XMC_CCU8_SLICE_COMPARE_CHANNEL_2, (uint16_t) 0);

	/* Transfer value from shadow timer registers to actual timer registers */
	XMC_CCU8_EnableShadowTransfer(module, shadow_transfer_enable_code);
	XMC_CCU8_EnableShadowTransfer(module, shadow_transfer_enable_dither_code);

	XMC_CCU8_SLICE_DeadTimeInit(slice, &PWM_CCU8_0_dt_config);

	/* Configure events */
	PWM_CCU8_lConfigure_Events(handle_ptr);

	/* Enable the interrupts */
	PWM_CCU8_lInit_Interrupt(handle_ptr);

	/*Initializes the GPIO*/
	PWM_CCU8_lInit_GPIO(handle_ptr);

	frequency_module = handle_ptr->config_ptr->global_ccu8_handle->module_frequency;
	prescalar = (uint32_t) handle_ptr->config_ptr->ccu8_cc8_slice_timer_ptr->prescaler_initval;
	frequency_module = frequency_module / ((uint32_t) 1 << prescalar);
	handle_ptr->frequency_tclk = frequency_module;

	handle_ptr->state = PWM_CCU8_STATE_INITIALIZED;

	/* Start the PWM generation if start at initialization is enabled */
	if ((bool) true == handle_ptr->config_ptr->start_control)
	{
		PWM_CCU8_Start(handle_ptr);
	}
}

static void PWM_CCU8_lInit_Interrupt(PWM_CCU8_t* handle_ptr)
{
	/* Enable events. Bind event to corresponding service request node. */

	XMC_CCU8_SLICE_SetInterruptNode(handle_ptr->ccu8_slice_ptr, XMC_CCU8_SLICE_IRQ_ID_PERIOD_MATCH,
			handle_ptr->config_ptr->sr_per_match);
	XMC_CCU8_SLICE_SetInterruptNode(handle_ptr->ccu8_slice_ptr, XMC_CCU8_SLICE_IRQ_ID_COMPARE_MATCH_UP_CH_1,
			handle_ptr->config_ptr->sr_cmp1_match_up);
	XMC_CCU8_SLICE_SetInterruptNode(handle_ptr->ccu8_slice_ptr, XMC_CCU8_SLICE_IRQ_ID_COMPARE_MATCH_DOWN_CH_1,
			handle_ptr->config_ptr->sr_cmp1_match_down);
	XMC_CCU8_SLICE_SetInterruptNode(handle_ptr->ccu8_slice_ptr, XMC_CCU8_SLICE_IRQ_ID_COMPARE_MATCH_UP_CH_2,
			handle_ptr->config_ptr->sr_cmp2_match_up);
	XMC_CCU8_SLICE_SetInterruptNode(handle_ptr->ccu8_slice_ptr, XMC_CCU8_SLICE_IRQ_ID_COMPARE_MATCH_DOWN_CH_2,
			handle_ptr->config_ptr->sr_cmp2_match_down);
	XMC_CCU8_SLICE_SetInterruptNode(handle_ptr->ccu8_slice_ptr, XMC_CCU8_SLICE_IRQ_ID_ONE_MATCH,
			handle_ptr->config_ptr->sr_one_match_down);
	XMC_CCU8_SLICE_SetInterruptNode(handle_ptr->ccu8_slice_ptr, XMC_CCU8_SLICE_IRQ_ID_EVENT0,
			handle_ptr->config_ptr->sr_e0);
	XMC_CCU8_SLICE_SetInterruptNode(handle_ptr->ccu8_slice_ptr, XMC_CCU8_SLICE_IRQ_ID_EVENT1,
			handle_ptr->config_ptr->sr_e1);
	XMC_CCU8_SLICE_SetInterruptNode(handle_ptr->ccu8_slice_ptr, XMC_CCU8_SLICE_IRQ_ID_EVENT2,
			handle_ptr->config_ptr->sr_e2);

	if ((bool) true == handle_ptr->config_ptr->int_per_match)
	{
		XMC_DEBUG("PWM_CCU8_Init: Interrupt period match enable");
		XMC_CCU8_SLICE_EnableEvent(handle_ptr->ccu8_slice_ptr, XMC_CCU8_SLICE_IRQ_ID_PERIOD_MATCH);
	}

	if ((bool) true == handle_ptr->config_ptr->int_cmp1_match_up)
	{
		XMC_DEBUG("PWM_CCU8_Init: Interrupt compare match ch1 up enable");
		XMC_CCU8_SLICE_EnableEvent(handle_ptr->ccu8_slice_ptr, XMC_CCU8_SLICE_IRQ_ID_COMPARE_MATCH_UP_CH_1);
	}

	if ((bool) true == handle_ptr->config_ptr->int_cmp1_match_down)
	{
		XMC_DEBUG("PWM_CCU8_Init: Interrupt compare match ch1 down enable");
		XMC_CCU8_SLICE_EnableEvent(handle_ptr->ccu8_slice_ptr, XMC_CCU8_SLICE_IRQ_ID_COMPARE_MATCH_DOWN_CH_1);
	}

	if ((bool) true == handle_ptr->config_ptr->int_cmp2_match_up)
	{
		XMC_DEBUG("PWM_CCU8_Init: Interrupt compare match ch2 up enable");
		XMC_CCU8_SLICE_EnableEvent(handle_ptr->ccu8_slice_ptr, XMC_CCU8_SLICE_IRQ_ID_COMPARE_MATCH_UP_CH_2);
	}

	if ((bool) true == handle_ptr->config_ptr->int_cmp2_match_down)
	{
		XMC_DEBUG("PWM_CCU8_Init: Interrupt compare match ch2 down enable");
		XMC_CCU8_SLICE_EnableEvent(handle_ptr->ccu8_slice_ptr, XMC_CCU8_SLICE_IRQ_ID_COMPARE_MATCH_DOWN_CH_2);
	}

	if ((bool) true == handle_ptr->config_ptr->int_one_match_down)
	{
		XMC_DEBUG("PWM_CCU8_Init: Interrupt one match enable");
		XMC_CCU8_SLICE_EnableEvent(handle_ptr->ccu8_slice_ptr, XMC_CCU8_SLICE_IRQ_ID_ONE_MATCH);
	}

	if ((bool) true == handle_ptr->config_ptr->int_e0)
	{
		XMC_DEBUG("PWM_CCU8_Init: Interrupt event 0 enable");
		XMC_CCU8_SLICE_EnableEvent(handle_ptr->ccu8_slice_ptr, XMC_CCU8_SLICE_IRQ_ID_EVENT0);
	}

	if ((bool) true == handle_ptr->config_ptr->int_e1)
	{
		XMC_DEBUG("PWM_CCU8_Init: Interrupt event 1 enable");
		XMC_CCU8_SLICE_EnableEvent(handle_ptr->ccu8_slice_ptr, XMC_CCU8_SLICE_IRQ_ID_EVENT1);
	}

	if ((bool) true == handle_ptr->config_ptr->int_e2)
	{
		XMC_DEBUG("PWM_CCU8_Init: Interrupt event 2 enable");
		XMC_CCU8_SLICE_EnableEvent(handle_ptr->ccu8_slice_ptr, XMC_CCU8_SLICE_IRQ_ID_EVENT2);
	}
}
static void PWM_CCU8_lInit_GPIO(PWM_CCU8_t* handle_ptr)
{
	if ((bool) true == handle_ptr->config_ptr->gpio_ch1_out0_enable)
	{
		XMC_GPIO_Init(handle_ptr->config_ptr->gpio_ch1_out0_ptr, handle_ptr->config_ptr->gpio_ch1_out0_pin,
				handle_ptr->config_ptr->gpio_ch1_out0_config_ptr);
	}

	if ((bool) true == handle_ptr->config_ptr->gpio_ch1_out1_enable)
	{
		XMC_GPIO_Init(handle_ptr->config_ptr->gpio_ch1_out1_ptr, handle_ptr->config_ptr->gpio_ch1_out1_pin,
				handle_ptr->config_ptr->gpio_ch1_out1_config_ptr);
	}

	if ((bool) true == handle_ptr->config_ptr->gpio_ch2_out2_enable)
	{
		XMC_GPIO_Init(handle_ptr->config_ptr->gpio_ch2_out2_ptr, handle_ptr->config_ptr->gpio_ch2_out2_pin,
				handle_ptr->config_ptr->gpio_ch2_out2_config_ptr);
	}

	if ((bool) true == handle_ptr->config_ptr->gpio_ch2_out3_enable)
	{
		XMC_GPIO_Init(handle_ptr->config_ptr->gpio_ch2_out3_ptr, handle_ptr->config_ptr->gpio_ch2_out3_pin,
				handle_ptr->config_ptr->gpio_ch2_out3_config_ptr);
	}
}

static void PWM_CCU8_lConfigure_Events(PWM_CCU8_t* handle_ptr)
{
	/* Configure slice to a external event 0 */
	XMC_CCU8_SLICE_ConfigureEvent(handle_ptr->ccu8_slice_ptr, XMC_CCU8_SLICE_EVENT_0,
			handle_ptr->config_ptr->event0_config_ptr);

	/* Configure slice to a external event 1 */
	XMC_CCU8_SLICE_ConfigureEvent(handle_ptr->ccu8_slice_ptr, XMC_CCU8_SLICE_EVENT_1,
			handle_ptr->config_ptr->event1_config_ptr);

	/* Configure slice to a external event 2 */
	XMC_CCU8_SLICE_ConfigureEvent(handle_ptr->ccu8_slice_ptr, XMC_CCU8_SLICE_EVENT_2,
			handle_ptr->config_ptr->event2_config_ptr);

	/* External signal controls start of the timer */
	if (XMC_CCU8_SLICE_EVENT_NONE != handle_ptr->config_ptr->ext_start_event)
	{
		XMC_CCU8_SLICE_StartConfig(handle_ptr->ccu8_slice_ptr, handle_ptr->config_ptr->ext_start_event,
				handle_ptr->config_ptr->ext_start_mode);
	}

	/* External signal can stop the timer */
	if (XMC_CCU8_SLICE_EVENT_NONE != handle_ptr->config_ptr->ext_stop_event)
	{
		XMC_CCU8_SLICE_StopConfig(handle_ptr->ccu8_slice_ptr, handle_ptr->config_ptr->ext_stop_event,
				handle_ptr->config_ptr->ext_stop_mode);
	}

	/* External signal can change the timer counting direction */
	if (XMC_CCU8_SLICE_EVENT_NONE != handle_ptr->config_ptr->ext_count_dir_event)
	{
		XMC_CCU8_SLICE_DirectionConfig(handle_ptr->ccu8_slice_ptr, handle_ptr->config_ptr->ext_count_dir_event);
	}
	/* External signal can stop the timer and the timer value remains same */
	if (XMC_CCU8_SLICE_EVENT_NONE != handle_ptr->config_ptr->ext_gate_event)
	{
		XMC_CCU8_SLICE_GateConfig(handle_ptr->ccu8_slice_ptr, handle_ptr->config_ptr->ext_gate_event);
	}
	/* Timer increments on external signal */
	if (XMC_CCU8_SLICE_EVENT_NONE != handle_ptr->config_ptr->ext_count_event)
	{
		XMC_CCU8_SLICE_CountConfig(handle_ptr->ccu8_slice_ptr, handle_ptr->config_ptr->ext_count_event);
	}
	/* Timer gets loaded with compare register value or period register value on external signal */
	if (XMC_CCU8_SLICE_EVENT_NONE != handle_ptr->config_ptr->ext_load_event)
	{
		XMC_CCU8_SLICE_LoadConfig(handle_ptr->ccu8_slice_ptr, handle_ptr->config_ptr->ext_load_event);
		XMC_CCU8_SLICE_LoadSelector(handle_ptr->ccu8_slice_ptr, handle_ptr->config_ptr->ext_load_selector);
	}
	/* External signal PWM signal (ST bit) output gets modulated by external signal */
	if (XMC_CCU8_SLICE_EVENT_NONE != handle_ptr->config_ptr->ext_mod_event)
	{
		XMC_CCU8_SLICE_ModulationConfig(handle_ptr->ccu8_slice_ptr, handle_ptr->config_ptr->ext_mod_event,
				handle_ptr->config_ptr->ext_mod_mode, XMC_CCU8_SLICE_MODULATION_CHANNEL_1_AND_2,
				handle_ptr->config_ptr->ext_mod_sync);
	}

	/* PWM signal (ST bit) output gets modulated by external signal */
	if (XMC_CCU8_SLICE_EVENT_2 == handle_ptr->config_ptr->ext_trap_event)
	{
		XMC_CCU8_SLICE_TrapConfig(handle_ptr->ccu8_slice_ptr, handle_ptr->config_ptr->ext_trap_exit,
				handle_ptr->config_ptr->ext_trap_sync);
		if ((bool) true == handle_ptr->config_ptr->ext_trap_enable)
		{
			XMC_CCU8_SLICE_EnableTrap(
					handle_ptr->ccu8_slice_ptr,
					(uint32_t)(XMC_CCU8_SLICE_OUTPUT_0 | XMC_CCU8_SLICE_OUTPUT_1 | XMC_CCU8_SLICE_OUTPUT_2 | XMC_CCU8_SLICE_OUTPUT_3)
			);
		}
	}

	if ((XMC_CCU8_SLICE_EVENT_NONE != handle_ptr->config_ptr->ext_override_edge_event) || (XMC_CCU8_SLICE_EVENT_NONE
			!= handle_ptr->config_ptr->ext_override_level_event))
	{
		XMC_CCU8_SLICE_ConfigureStatusBitOverrideEvent(handle_ptr->ccu8_slice_ptr,
				handle_ptr->config_ptr->event1_config_ptr,
				handle_ptr->config_ptr->event2_config_ptr);
		XMC_CCU8_SLICE_StatusBitOverrideConfig(handle_ptr->ccu8_slice_ptr);
	}

}
