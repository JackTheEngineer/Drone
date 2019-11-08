/**
 * @file pwm_ccu4.c
 * @date 2019-09-23
 *
 * NOTE:
 * This file is generated by DAVE. Any manual modification done to this file will be lost when the code is regenerated.
 *
 * @cond
 ***********************************************************************************************************************
 * PWM_CCU4 v4.1.26 - PWM APP using one timer slice of CCU4, with external events support, to generate a PWM output.
 *
 * Copyright (c) 2015 - 2019, Infineon Technologies AG
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,are permitted provided that the
 * following conditions are met:
 *
 *   Redistributions of source code must retain the above copyright notice, this list of conditions and the  following
 *   disclaimer.
 *
 *   Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following
 *   disclaimer in the documentation and/or other materials provided with the distribution.
 *
 *   Neither the name of the copyright holders nor the names of its contributors may be used to endorse or promote
 *   products derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE  FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY,OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT  OF THE
 * USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * To improve the quality of the software, users are encouraged to share modifications, enhancements or bug fixes
 * with Infineon Technologies AG (dave@infineon.com).
 ***********************************************************************************************************************
 *
 * Change History
 * --------------
 *
 * 2015-02-14:
 *     - Initial version for DAVEv4
 *
 * 2015-05-20:
 *     - PWM_CCU4_AcknowledgeInterrupt() api is renamed as PWM_CCU4_ClearEvent().
 *
 * 2015-06-20:
 *     - Copyright information updated.
 *
 * 2015-08-13:
 *     - Added support for selection of shadow transfer mode using API "XMC_CCU4_SLICE_SetShadowTransferMode"
 *     - Added support for selection of immediate shadow transfer using
 *       API "XMC_CCU4_SLICE_WriteImmediateAfterShadowTransfer" in XMC14* devices
 *     - Added support for automatic shadow transfer using
 *       API "XMC_CCU4_SLICE_EnableAutomaticShadowTransferRequest" in XMC14* devices
 *
 * 2016-10-28:
 *     - Changed PWM_CCU4_Stop() to set the ST output to passive value after stopping the timer.
 *
 * 2019-09-23:
 *     - Fixed escaping of '%' in XMC_DEBUG()
 *
 * @endcond
 *
 */

/***********************************************************************************************************************
 * HEADER FILES
 **********************************************************************************************************************/
#include "pwm_ccu4.h"

/***********************************************************************************************************************
 * MACROS
 **********************************************************************************************************************/

/***********************************************************************************************************************
 * LOCAL DATA
 **********************************************************************************************************************/

/***********************************************************************************************************************
 * LOCAL ROUTINES
 **********************************************************************************************************************/

/* Initialize the App Interrupts */
static void PWM_CCU4_lInit_Interrupt(PWM_CCU4_t* handle_ptr);

/* Initialize the App events and configurations */
static void PWM_CCU4_lConfigure_Events(PWM_CCU4_t* handle_ptr);

/**********************************************************************************************************************
 * API IMPLEMENTATION
 **********************************************************************************************************************/

/* API to retrieve App version info */
DAVE_APP_VERSION_t PWM_CCU4_GetAppVersion(void)
{
  DAVE_APP_VERSION_t version;

  version.major = PWM_CCU4_MAJOR_VERSION;
  version.minor = PWM_CCU4_MINOR_VERSION;
  version.patch = PWM_CCU4_PATCH_VERSION;

  return version;
}

/* This function initializes the app */
PWM_CCU4_STATUS_t PWM_CCU4_Init(PWM_CCU4_t* handle_ptr)
{
  PWM_CCU4_STATUS_t status;
  GLOBAL_CCU4_STATUS_t status_ccu4_global;
  uint32_t frequency_module;
  uint32_t prescalar;

  status = PWM_CCU4_STATUS_FAILURE;
  status_ccu4_global = GLOBAL_CCU4_STATUS_FAILURE;
  XMC_ASSERT("PWM_CCU4_Init:handle_ptr is NULL", (handle_ptr != NULL));

  if (PWM_CCU4_STATE_UNINITIALIZED == handle_ptr->state)
  {
    /* Initialize consumed Apps */
    status_ccu4_global = GLOBAL_CCU4_Init(handle_ptr->config_ptr->global_ccu4_handle);

    /* Initialize CCU4x_CC4y slice */
    if (GLOBAL_CCU4_STATUS_SUCCESS == status_ccu4_global)
    {
      XMC_DEBUG("PWM_CCU4_Init:Initilizing slice");

      /* Configure CCU4x_CC4y slice as timer */
      XMC_CCU4_SLICE_CompareInit(handle_ptr->ccu4_slice_ptr, handle_ptr->config_ptr->ccu4_cc4_slice_timer_ptr);
      /* Set period match value of the timer  */
      XMC_CCU4_SLICE_SetTimerPeriodMatch(handle_ptr->ccu4_slice_ptr, handle_ptr->config_ptr->period_value);

      /* Set timer compare match value for channel 1 */
      XMC_CCU4_SLICE_SetTimerCompareMatch(handle_ptr->ccu4_slice_ptr, (uint16_t) handle_ptr->config_ptr->compare_value);

      if (1U == handle_ptr->config_ptr->ccu4_cc4_slice_timer_ptr->mcm_enable)
      {
        XMC_CCU4_SetMultiChannelShadowTransferMode(handle_ptr->ccu4_module_ptr,
                                                   (uint32_t) handle_ptr->config_ptr->mcm_shadow_txfr_mode);
      }

#if (UC_SERIES == XMC14) /*below feature available in XMC14xx devices */
      XMC_CCU4_SLICE_SetShadowTransferMode(handle_ptr->ccu4_slice_ptr, handle_ptr->config_ptr->shadow_transfer_mode);
      XMC_CCU4_SLICE_WriteImmediateAfterShadowTransfer(handle_ptr->ccu4_slice_ptr,
                                                       handle_ptr->config_ptr->immediate_write);
      XMC_CCU4_SLICE_EnableAutomaticShadowTransferRequest(handle_ptr->ccu4_slice_ptr,
                                                          handle_ptr->config_ptr->automatic_shadow_transfer);
      if((bool)true == handle_ptr->config_ptr->cascaded_shadow_txfr_enable)
      {
        XMC_CCU4_SLICE_EnableCascadedShadowTransfer(handle_ptr->ccu4_slice_ptr);
      }
#endif

      /* Transfer value from shadow timer registers to actual timer registers */
      XMC_CCU4_EnableShadowTransfer(handle_ptr->ccu4_module_ptr, handle_ptr->shadow_txfr_msk);
      XMC_CCU4_EnableShadowTransfer(handle_ptr->ccu4_module_ptr, handle_ptr->dither_shadow_txfr_msk);

      /* Configure events */
      PWM_CCU4_lConfigure_Events(handle_ptr);

      /* Enable the interrupts */
      PWM_CCU4_lInit_Interrupt(handle_ptr);

      /*Initializes the GPIO*/
      if ((bool) true == handle_ptr->config_ptr->gpio_ch_out_enable)
      {
        XMC_GPIO_Init(handle_ptr->config_ptr->gpio_ch_out_ptr, handle_ptr->config_ptr->gpio_ch_out_pin,
                      handle_ptr->config_ptr->gpio_ch_out_config_ptr);
      }

      frequency_module = handle_ptr->config_ptr->global_ccu4_handle->module_frequency;
      prescalar = (uint32_t) handle_ptr->config_ptr->ccu4_cc4_slice_timer_ptr->prescaler_initval;
      frequency_module = frequency_module / ((uint32_t) 1 << prescalar);
      handle_ptr->frequency_tclk = frequency_module;

      handle_ptr->state = PWM_CCU4_STATE_INITIALIZED;
      status = PWM_CCU4_STATUS_SUCCESS;

      /* Start the PWM generation if start at initialization is enabled */
      if ((bool) true == handle_ptr->config_ptr->start_control)
      {
        status = PWM_CCU4_Start(handle_ptr);
      }
    }
    else
    {
      handle_ptr->state = PWM_CCU4_STATE_UNINITIALIZED;
    }

  }
  else
  {
    status = PWM_CCU4_STATUS_ALREADY_INITIALIZED;
    XMC_DEBUG("PWM_CCU4_Init:PWM_CCU4_STATUS_ALREADY_INITIALIZED");
  }

  return (status);
} /* end of PWM_CCU4_Init() api */

static void PWM_CCU4_lInit_Interrupt(PWM_CCU4_t* handle_ptr)
{

  /* Enable events. Bind event to corresponding service request node.Enable Interrupts. The user may choose to 
   disable the interrupts by LLD calls. */
  if ((bool) true == handle_ptr->config_ptr->int_per_match)
  {
    XMC_DEBUG("PWM_CCU4_Init: Interrupt period match enable");
    XMC_CCU4_SLICE_SetInterruptNode(handle_ptr->ccu4_slice_ptr, XMC_CCU4_SLICE_IRQ_ID_PERIOD_MATCH,
                                    handle_ptr->config_ptr->sr_per_match);
    XMC_CCU4_SLICE_EnableEvent(handle_ptr->ccu4_slice_ptr, XMC_CCU4_SLICE_IRQ_ID_PERIOD_MATCH);
  }

  if ((bool) true == handle_ptr->config_ptr->int_cmp_match_up)
  {
    XMC_DEBUG("PWM_CCU4_Init: Interrupt compare match up enable");
    XMC_CCU4_SLICE_SetInterruptNode(handle_ptr->ccu4_slice_ptr, XMC_CCU4_SLICE_IRQ_ID_COMPARE_MATCH_UP,
                                    handle_ptr->config_ptr->sr_cmp_match_up);
    XMC_CCU4_SLICE_EnableEvent(handle_ptr->ccu4_slice_ptr, XMC_CCU4_SLICE_IRQ_ID_COMPARE_MATCH_UP);
  }

  if ((bool) true == handle_ptr->config_ptr->int_cmp_match_down)
  {
    XMC_DEBUG("PWM_CCU4_Init: Interrupt compare match down enable");
    XMC_CCU4_SLICE_SetInterruptNode(handle_ptr->ccu4_slice_ptr, XMC_CCU4_SLICE_IRQ_ID_COMPARE_MATCH_DOWN,
                                    handle_ptr->config_ptr->sr_cmp_match_down);
    XMC_CCU4_SLICE_EnableEvent(handle_ptr->ccu4_slice_ptr, XMC_CCU4_SLICE_IRQ_ID_COMPARE_MATCH_DOWN);
  }

  if ((bool) true == handle_ptr->config_ptr->int_one_match_down)
  {
    XMC_DEBUG("PWM_CCU4_Init: Interrupt one match enable");
    XMC_CCU4_SLICE_SetInterruptNode(handle_ptr->ccu4_slice_ptr, XMC_CCU4_SLICE_IRQ_ID_ONE_MATCH,
                                    handle_ptr->config_ptr->sr_one_match_down);
    XMC_CCU4_SLICE_EnableEvent(handle_ptr->ccu4_slice_ptr, XMC_CCU4_SLICE_IRQ_ID_ONE_MATCH);
  }

  if ((bool) true == handle_ptr->config_ptr->int_e0)
  {
    XMC_DEBUG("PWM_CCU4_Init: Interrupt event 0 enable");
    XMC_CCU4_SLICE_SetInterruptNode(handle_ptr->ccu4_slice_ptr, XMC_CCU4_SLICE_IRQ_ID_EVENT0,
                                    handle_ptr->config_ptr->sr_e0);
    XMC_CCU4_SLICE_EnableEvent(handle_ptr->ccu4_slice_ptr, XMC_CCU4_SLICE_IRQ_ID_EVENT0);
  }

  if ((bool) true == handle_ptr->config_ptr->int_e1)
  {
    XMC_DEBUG("PWM_CCU4_Init: Interrupt event 1 enable");
    XMC_CCU4_SLICE_SetInterruptNode(handle_ptr->ccu4_slice_ptr, XMC_CCU4_SLICE_IRQ_ID_EVENT1,
                                    handle_ptr->config_ptr->sr_e1);
    XMC_CCU4_SLICE_EnableEvent(handle_ptr->ccu4_slice_ptr, XMC_CCU4_SLICE_IRQ_ID_EVENT1);
  }

  if ((bool) true == handle_ptr->config_ptr->int_e2)
  {
    XMC_DEBUG("PWM_CCU4_Init: Interrupt event 2 enable");
    XMC_CCU4_SLICE_SetInterruptNode(handle_ptr->ccu4_slice_ptr, XMC_CCU4_SLICE_IRQ_ID_EVENT2,
                                    handle_ptr->config_ptr->sr_e2);
    XMC_CCU4_SLICE_EnableEvent(handle_ptr->ccu4_slice_ptr, XMC_CCU4_SLICE_IRQ_ID_EVENT2);
  }
}

static void PWM_CCU4_lConfigure_Events(PWM_CCU4_t* handle_ptr)
{

  /* Configure slice to a external event 0 */
  XMC_CCU4_SLICE_ConfigureEvent(handle_ptr->ccu4_slice_ptr, XMC_CCU4_SLICE_EVENT_0,
                                handle_ptr->config_ptr->event0_config_ptr);

  /* Configure slice to a external event 1 */
  XMC_CCU4_SLICE_ConfigureEvent(handle_ptr->ccu4_slice_ptr, XMC_CCU4_SLICE_EVENT_1,
                                handle_ptr->config_ptr->event1_config_ptr);

  /* Configure slice to a external event 2 */
  XMC_CCU4_SLICE_ConfigureEvent(handle_ptr->ccu4_slice_ptr, XMC_CCU4_SLICE_EVENT_2,
                                handle_ptr->config_ptr->event2_config_ptr);

  /* External signal controls start of the timer */
  if (XMC_CCU4_SLICE_EVENT_NONE != handle_ptr->config_ptr->ext_start_event)
  {
    XMC_CCU4_SLICE_StartConfig(handle_ptr->ccu4_slice_ptr, handle_ptr->config_ptr->ext_start_event,
                               handle_ptr->config_ptr->ext_start_mode);
  }

  /* External signal can stop the timer */
  if (XMC_CCU4_SLICE_EVENT_NONE != handle_ptr->config_ptr->ext_stop_event)
  {
    XMC_CCU4_SLICE_StopConfig(handle_ptr->ccu4_slice_ptr, handle_ptr->config_ptr->ext_stop_event,
                              handle_ptr->config_ptr->ext_stop_mode);
  }

  /* External signal can change the timer counting direction */
  if (XMC_CCU4_SLICE_EVENT_NONE != handle_ptr->config_ptr->ext_count_dir_event)
  {
    XMC_CCU4_SLICE_DirectionConfig(handle_ptr->ccu4_slice_ptr, handle_ptr->config_ptr->ext_count_dir_event);
  }
  /* External signal can stop the timer and the timer value remains same */
  if (XMC_CCU4_SLICE_EVENT_NONE != handle_ptr->config_ptr->ext_gate_event)
  {
    XMC_CCU4_SLICE_GateConfig(handle_ptr->ccu4_slice_ptr, handle_ptr->config_ptr->ext_gate_event);
  }
  /* Timer increments on external signal */
  if (XMC_CCU4_SLICE_EVENT_NONE != handle_ptr->config_ptr->ext_count_event)
  {
    XMC_CCU4_SLICE_CountConfig(handle_ptr->ccu4_slice_ptr, handle_ptr->config_ptr->ext_count_event);
  }
  /* Timer gets loaded with compare register value or period register value on external signal */
  if (XMC_CCU4_SLICE_EVENT_NONE != handle_ptr->config_ptr->ext_load_event)
  {
    XMC_CCU4_SLICE_LoadConfig(handle_ptr->ccu4_slice_ptr, handle_ptr->config_ptr->ext_load_event);
  }
  /* External signal PWM signal (ST bit) output gets modulated by external signal */
  if (XMC_CCU4_SLICE_EVENT_NONE != handle_ptr->config_ptr->ext_mod_event)
  {
    XMC_CCU4_SLICE_ModulationConfig(handle_ptr->ccu4_slice_ptr, handle_ptr->config_ptr->ext_mod_event,
                                    handle_ptr->config_ptr->ext_mod_mode, handle_ptr->config_ptr->ext_mod_sync);
  }

  /* PWM signal (ST bit) output gets modulated by external signal */
  if (XMC_CCU4_SLICE_EVENT_2 == handle_ptr->config_ptr->ext_trap_event)
  {
    XMC_CCU4_SLICE_TrapConfig(handle_ptr->ccu4_slice_ptr, handle_ptr->config_ptr->ext_trap_exit,
                              handle_ptr->config_ptr->ext_trap_sync);

    if ((bool) true == handle_ptr->config_ptr->ext_trap_enable)
    {
      XMC_CCU4_SLICE_EnableTrap(handle_ptr->ccu4_slice_ptr);
    }
  }
  if ((XMC_CCU4_SLICE_EVENT_1 == handle_ptr->config_ptr->ext_override_edge_event) && (XMC_CCU4_SLICE_EVENT_2
      == handle_ptr->config_ptr->ext_override_level_event))
  {
    XMC_CCU4_SLICE_ConfigureStatusBitOverrideEvent(handle_ptr->ccu4_slice_ptr,
                                                   handle_ptr->config_ptr->event1_config_ptr,
                                                   handle_ptr->config_ptr->event2_config_ptr);
    XMC_CCU4_SLICE_StatusBitOverrideConfig(handle_ptr->ccu4_slice_ptr);
  }

}
/**********************************************************************************************************/
/*Starts the CCU4_CC4 slice. This needs to be called even if external start is configured.*/
PWM_CCU4_STATUS_t PWM_CCU4_Start(PWM_CCU4_t* handle_ptr)
{
  PWM_CCU4_STATUS_t status;

  status = PWM_CCU4_STATUS_FAILURE;
  XMC_ASSERT("PWM_CCU4_Start:handle_ptr NULL", (handle_ptr != NULL));
  if ((PWM_CCU4_STATE_INITIALIZED == handle_ptr->state) || (PWM_CCU4_STATE_STOPPED == handle_ptr->state))
  {
    /* clear IDLE mode for the slice; Start timer */
    XMC_CCU4_EnableClock(handle_ptr->ccu4_module_ptr, handle_ptr->slice_number);

    if (XMC_CCU4_SLICE_EVENT_NONE == handle_ptr->config_ptr->ext_start_event)
    {
      XMC_CCU4_SLICE_StartTimer(handle_ptr->ccu4_slice_ptr);
    }

    handle_ptr->state = PWM_CCU4_STATE_RUNNING;
    status = PWM_CCU4_STATUS_SUCCESS;
    XMC_DEBUG("PWM_CCU4_Start:start PWM");
  }
  return (status);
} /* end of PWM_CCU4_Start() api */
/**********************************************************************************************************/
/*Stops the CCU4_CC4 slice. */
PWM_CCU4_STATUS_t PWM_CCU4_Stop(PWM_CCU4_t* handle_ptr)
{
  PWM_CCU4_STATUS_t status;

  status = PWM_CCU4_STATUS_FAILURE;
  XMC_ASSERT("PWM_CCU4_Stop:handle_ptr NULL", (handle_ptr != NULL));
  if (PWM_CCU4_STATE_UNINITIALIZED != handle_ptr->state)
  {
    XMC_CCU4_SLICE_StopTimer(handle_ptr->ccu4_slice_ptr);
    XMC_CCU4_SLICE_ClearTimer(handle_ptr->ccu4_slice_ptr);

    handle_ptr->state = PWM_CCU4_STATE_STOPPED;
    status = PWM_CCU4_STATUS_SUCCESS;
    XMC_DEBUG("PWM_CCU4_Stop:stop PWM");
  }
  return (status);

} /* end of PWM_CCU4_Stop() api */
/**********************************************************************************************************/
/*Gets the timer value of  CCU4_CC4 slice. */
uint32_t PWM_CCU4_GetTimerValue(PWM_CCU4_t* handle_ptr)
{
  uint32_t timer_value;
  XMC_ASSERT("PWM_CCU4_GetTimerValue:handle_ptr NULL", (handle_ptr != NULL));
  timer_value = (uint32_t) XMC_CCU4_SLICE_GetTimerValue(handle_ptr->ccu4_slice_ptr);
  XMC_DEBUG("PWM_CCU4_GetTimerValue:timer value");
  return (timer_value);
}/* end of PWM_CCU4_GetTimerValue() api */
/**********************************************************************************************************/
/*Gets the status of  CCU4_CC4 slice. */
bool PWM_CCU4_GetTimerStatus(PWM_CCU4_t* handle_ptr)
{
  bool status_timer;
  XMC_ASSERT("PWM_CCU4_GetTimerStatus:handle_ptr NULL", (handle_ptr != NULL));
  status_timer = XMC_CCU4_SLICE_IsTimerRunning(handle_ptr->ccu4_slice_ptr);
  return (status_timer);

} /* end of PWM_CCU4_GetStatus() api */

/**********************************************************************************************************/

/*Sets the frequency for CCU4_CC4 slice. */
PWM_CCU4_STATUS_t PWM_CCU4_SetFreq(PWM_CCU4_t* handle_ptr, uint32_t pwm_freq_hz)
{
  PWM_CCU4_STATUS_t status;
  uint32_t frequency_tclk;
  uint32_t period;
  uint32_t duty;
  uint16_t compare;

  status = PWM_CCU4_STATUS_FAILURE;
  frequency_tclk = 0U;
  XMC_ASSERT("PWM_CCU4_SetFreq:handle_ptr NULL", (handle_ptr != NULL));
  if (PWM_CCU4_STATE_UNINITIALIZED != handle_ptr->state)
  {
    if (0U == pwm_freq_hz)
    {
      XMC_DEBUG("PWM_CCU4_SetFreq:cannot set frequency 0Hz");
    }
    else
    {
      frequency_tclk = handle_ptr->frequency_tclk;
      period = frequency_tclk / pwm_freq_hz;

      if ((uint32_t) XMC_CCU4_SLICE_TIMER_COUNT_MODE_CA == handle_ptr->config_ptr->ccu4_cc4_slice_timer_ptr->timer_mode)
      {
        period = period >> 1U;/*divide by 2*/
      }

      if ((period != 0U) && (period <= PWM_CCU4_MAX_TIMER_COUNT))
      {
        /*Calculate the current duty cycle in no-timer concatenation mode*/
        duty = handle_ptr->sym_duty;

        duty = (PWM_CCU4_DUTY_FULL_SCALE - duty);
        duty = duty * period;
        duty = duty / PWM_CCU4_DUTY_FULL_SCALE;

        compare = (uint16_t) duty;

        XMC_CCU4_SLICE_SetTimerPeriodMatch(handle_ptr->ccu4_slice_ptr, (uint16_t)(period - 1U));
        XMC_CCU4_SLICE_SetTimerCompareMatch(handle_ptr->ccu4_slice_ptr, compare);
        XMC_CCU4_EnableShadowTransfer(handle_ptr->ccu4_module_ptr, handle_ptr->shadow_txfr_msk);
        XMC_DEBUG("PWM_CCU4_SetFreq:frequency set");
        status = PWM_CCU4_STATUS_SUCCESS;
      }
    }
  }
  return (status);

} /* end of PWM_CCU4_SetFreqSymmetric() api */

/**********************************************************************************************************/

/*Sets the duty cycle (uint32_t) for CCU4_CC4 slice. */
PWM_CCU4_STATUS_t PWM_CCU4_SetDutyCycle(PWM_CCU4_t* handle_ptr, uint32_t duty_cycle)
{
  PWM_CCU4_STATUS_t status;
  uint32_t period;
  uint32_t compare;

  status = PWM_CCU4_STATUS_FAILURE;
  XMC_ASSERT("PWM_CCU4_SetDutyCycle:handle_ptr NULL", (handle_ptr != NULL));
  if (PWM_CCU4_STATE_UNINITIALIZED != handle_ptr->state)
  {
    if ((duty_cycle > PWM_CCU4_SYM_DUTY_MAX))
    {
      XMC_DEBUG("PWM_CCU4_SetDutyCycle:Cannot set duty cycle > 100%%");
    }
    else
    {
      period = (uint32_t) XMC_CCU4_SLICE_GetTimerPeriodMatch(handle_ptr->ccu4_slice_ptr) + 1U;

      /* Duty Cycle(symmetric) = (PR-CR1)+1 / period */
      compare = ((period * (PWM_CCU4_DUTY_FULL_SCALE - duty_cycle)) / PWM_CCU4_DUTY_FULL_SCALE);

      XMC_CCU4_SLICE_SetTimerCompareMatch(handle_ptr->ccu4_slice_ptr, (uint16_t) compare);
      XMC_CCU4_EnableShadowTransfer(handle_ptr->ccu4_module_ptr, handle_ptr->shadow_txfr_msk);

      handle_ptr->sym_duty = duty_cycle;

      XMC_DEBUG("PWM_CCU4_SetDutyCycle:dutycycle set");
      status = PWM_CCU4_STATUS_SUCCESS;
    }
  }
  return (status);
} /* end of PWM_CCU4_SetDutyCycle() api */

/**********************************************************************************************************/

/*Sets the frequency and duty cycle for CCU4_CC4 slice Symmetric Mode. */
PWM_CCU4_STATUS_t PWM_CCU4_SetFreqAndDutyCycle(PWM_CCU4_t* handle_ptr, uint32_t pwm_freq_hz, uint32_t duty)
{

  PWM_CCU4_STATUS_t status;
  uint32_t frequency_tclk;
  uint32_t period;
  uint32_t compare;

  status = PWM_CCU4_STATUS_FAILURE;
  frequency_tclk = 0U;
  XMC_ASSERT("PWM_CCU4_SetFreqAndDutyCycle:handle_ptr NULL", (handle_ptr != NULL));
  if (PWM_CCU4_STATE_UNINITIALIZED != handle_ptr->state)
  {
    if (0U == pwm_freq_hz)
    {
      XMC_DEBUG("PWM_CCU4_SetFreqAndDutyCycleSymmetric:cannot set frequency 0Hz");
    }
    else if (duty > PWM_CCU4_SYM_DUTY_MAX)
    {
      XMC_DEBUG("PWM_CCU4_SetFreqAndDutyCycle:duty > 100%%");
    }
    else
    {
      frequency_tclk = handle_ptr->frequency_tclk;
      period = frequency_tclk / pwm_freq_hz;

      if ((uint32_t) XMC_CCU4_SLICE_TIMER_COUNT_MODE_CA == handle_ptr->config_ptr->ccu4_cc4_slice_timer_ptr->timer_mode)
      {
        period = period >> 1U;/*divide by 2*/
      }

      if ((period != 0U) && (period <= PWM_CCU4_MAX_TIMER_COUNT))
      {
        /*Calculate the current duty cycle in no-timer concatenation mode*/
        compare = ((period * (PWM_CCU4_DUTY_FULL_SCALE - duty)) / PWM_CCU4_DUTY_FULL_SCALE);

        XMC_CCU4_SLICE_SetTimerPeriodMatch(handle_ptr->ccu4_slice_ptr, (uint16_t)(period - 1U));
        XMC_CCU4_SLICE_SetTimerCompareMatch(handle_ptr->ccu4_slice_ptr, (uint16_t) compare);

        XMC_CCU4_EnableShadowTransfer(handle_ptr->ccu4_module_ptr, handle_ptr->shadow_txfr_msk);

        handle_ptr->sym_duty = duty;

        XMC_DEBUG("PWM_CCU4_SetFreqAndDutyCycle:frequency set");
        status = PWM_CCU4_STATUS_SUCCESS;
      }
    }
  }
  return (status);

}/* end of PWM_CCU4_SetFreqAndDutyCycle() api */

/**********************************************************************************************************/

/*Sets the dither value, enables the dither. */
void PWM_CCU4_SetDither(PWM_CCU4_t* handle_ptr, bool dither_period, bool dither_comp, uint8_t dither_value)
{

  XMC_ASSERT("PWM_CCU4_SetDither:handle_ptr NULL", (handle_ptr != NULL));
  XMC_CCU4_SLICE_EnableDithering(handle_ptr->ccu4_slice_ptr, dither_period, dither_comp, dither_value);
  XMC_CCU4_EnableShadowTransfer(handle_ptr->ccu4_module_ptr, handle_ptr->dither_shadow_txfr_msk);
  XMC_DEBUG("PWM_CCU4_SetDither:dither compare value set");

}/* end of PWM_CCU4_SetDither() api */

/**********************************************************************************************************/

/*exits trap condition if trap signal is inactive */
void PWM_CCU4_ClearTrap(PWM_CCU4_t* handle_ptr)
{

  XMC_ASSERT("PWM_CCU4_ClearTrap:handle_ptr NULL", (handle_ptr != NULL));
  XMC_CCU4_SLICE_ClearEvent(handle_ptr->ccu4_slice_ptr, XMC_CCU4_SLICE_IRQ_ID_EVENT2);
  XMC_DEBUG("PWM_CCU4_ClearTrap:trap event cleared");

}/* end of PWM_CCU4_ClearTrap() api */

/**********************************************************************************************************/

/*Gets the interrupt status of  CCU4_CC4 slice. */
bool PWM_CCU4_GetInterruptStatus(PWM_CCU4_t* handle_ptr, XMC_CCU4_SLICE_IRQ_ID_t pwm_interrupt)
{
  bool status = (bool) false;
  XMC_ASSERT("PWM_CCU4_GetInterruptStatus:handle_ptr NULL", (handle_ptr != NULL));
  status = XMC_CCU4_SLICE_GetEvent(handle_ptr->ccu4_slice_ptr, pwm_interrupt);
  return (status);
} /* end of PWM_CCU4_GetInterruptStatus() api */

/**********************************************************************************************************/

/*Acknowledges the interrupt of  CCU4_CC4 slice. */
void PWM_CCU4_ClearEvent(PWM_CCU4_t* handle_ptr, XMC_CCU4_SLICE_IRQ_ID_t pwm_interrupt)
{
  XMC_ASSERT("PWM_CCU4_ClearEvent:handle_ptr NULL", (handle_ptr != NULL));
  XMC_CCU4_SLICE_ClearEvent(handle_ptr->ccu4_slice_ptr, pwm_interrupt);
  XMC_DEBUG("PWM_CCU4_ClearEvent:Acknowledge Interrupt");
} /* end of PWM_CCU4_ClearEvent() api */

/* end of CCU4 function definitions */

