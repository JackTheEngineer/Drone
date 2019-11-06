/**
 * @file pwm_ccu4.h
 * @date 2016-03-21
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
 *     - Logic for LLD version check is added.
 *
 * 2015-06-19:
 *     - Logic for LLD version check is removed.
 *     - LLD package version check is added.
 *
 * 2015-06-20:
 *     - Copyright information updated.
 *
 * 2015-06-23
 *     - In comment section "comapre" correctly spelt as "compare"
 *
 * 2015-08-13:
 *     - Added "immediate_write" member in  "PWM_CCU4_CONFIG_t"
 *     - Added "automatic_shadow_transfer" member in  "PWM_CCU4_CONFIG_t"
 *     - Added "shadow_transfer_mode" member in "PWM_CCU4_CONFIG_t"
 * @endcond
 *
 */

#ifndef PWM_CCU4_H_
#define PWM_CCU4_H_

/***********************************************************************************************************************
 * HEADER FILES
 **********************************************************************************************************************/
#include <xmc_gpio.h>
#include "pwm_ccu4_conf.h"

#if (!((XMC_LIB_MAJOR_VERSION == 2U) && \
       (XMC_LIB_MINOR_VERSION >= 0U) && \
       (XMC_LIB_PATCH_VERSION >= 0U)))
#error "PWM_CCU4 requires XMC Peripheral Library v2.0.0 or higher"
#endif

/**********************************************************************************************************************
* MACROS
**********************************************************************************************************************/

#define PWM_CCU4_MAX_TIMER_COUNT  (65535U)

#define PWM_CCU4_DUTY_FULL_SCALE    (10000U) /*100% * 100*/
#define PWM_CCU4_DUTY_SCALE         (100U)   /*100*/

#define PWM_CCU4_SYM_DUTY_MAX       (10000U) /*duty Max*/
#define PWM_CCU4_SYM_DUTY_MIN       (0U)     /*duty Min*/

/**********************************************************************************************************************
* ENUMS
**********************************************************************************************************************/

 /**
  * @ingroup PWM_CCU4_enumerations
  * @{
  */

/**
 *   @brief The type identifies the APP status.
 */
typedef enum PWM_CCU4_STATUS
{
  /**
  * STATUS SUCCESS
  */
  PWM_CCU4_STATUS_SUCCESS = 0U,

  /**
  * STATUS FAILURE
  */
  PWM_CCU4_STATUS_FAILURE = 1U,

  /**
  * STATUS ALREADY INITIALIZED
  */
  PWM_CCU4_STATUS_ALREADY_INITIALIZED = 2U
} PWM_CCU4_STATUS_t;


/**
 * @brief The type identifies APP state.
 */
typedef enum PWM_CCU4_STATE
{
  /**
   * default state after power on reset
   * PWM_CCU4 APP is in uninitialized mode. The corresponding CCU4 timer is not configured.
   * PWM pulses is not generated.
   */
  PWM_CCU4_STATE_UNINITIALIZED = 0U,

  /**
   * PWM_CCU4 APP is in initialized mode. The corresponding CCU4 timer is configured.
   * The corresponding CCU4 timer is not started(not running).
   */
  PWM_CCU4_STATE_INITIALIZED = 1U,

  /**
   * PWM_CCU4 APP is in running mode. The corresponding CCU4 timer is running.
   * Trigger signal for any of the configured Interrupt or service request in the CCU4 timer is triggered.
   */
  PWM_CCU4_STATE_RUNNING = 2U,

  /**
   * PWM_CCU4 APP is in stopped mode. The corresponding CCU4 timer is stopped.
   * Trigger signal for any of the configured Interrupt or service request in the CCU4 timer is not triggered.
   */
  PWM_CCU4_STATE_STOPPED = 3U

} PWM_CCU4_STATE_t;

/**
 * @}
 */

/**********************************************************************************************************************
* DATA STRUCTURES
**********************************************************************************************************************/
/**
  * @ingroup PWM_CCU4_datastructures
  * @{
  */
/**
 * @brief Configuration parameters of the PWM_CCU4 APP
 */
typedef struct PWM_CCU4_ConfigType
{
    const    bool                                   start_control;            /**<Enables starting of timer after initialization*/
    const    uint16_t                               period_value;             /**<Period register value. Determines the frequency*/
    const    uint16_t                               compare_value;            /**<Channel 1 compare register value. Determines the duty cycle*/

    const    bool                                   int_per_match;            /**<Enables event service request generation when timer value equals to period register */
    const    bool                                   int_cmp_match_up;         /**<Enables event service request generation when timer is counting up and equals to channel 1 compare register*/
    const    bool                                   int_cmp_match_down;       /**<Enables event service request generation when timer is counting down and equals to channel 1 compare register*/
    const    bool                                   int_one_match_down;       /**<Enables event service request generation when timer is counting down and equals 1*/
    const    bool                                   int_e0;                   /**<Enables event service request generation by external event 0 signal based on the trigger edge selection */
    const    bool                                   int_e1;                   /**<Enables event service request generation by external event 1 signal based on the trigger edge selection */
    const    bool                                   int_e2;                   /**<Enables event service request generation by external event 2 signal based on the trigger edge selection */

    const    XMC_CCU4_SLICE_SR_ID_t                 sr_per_match;             /**<Service request node to which  period match event is forwarded*/
    const    XMC_CCU4_SLICE_SR_ID_t                 sr_cmp_match_up;          /**<Service request node to which  channel 1 compare match while timer counting up event is forwarded*/
    const    XMC_CCU4_SLICE_SR_ID_t                 sr_cmp_match_down;        /**<Service request node to which  channel 1 compare match while timer counting down event is forwarded*/
    const    XMC_CCU4_SLICE_SR_ID_t                 sr_one_match_down;        /**<Service request node to which  timer one match event is forwarded*/
    const    XMC_CCU4_SLICE_SR_ID_t                 sr_e0;                    /**<Service request node to which  event 0 is forwarded*/
    const    XMC_CCU4_SLICE_SR_ID_t                 sr_e1;                    /**<Service request node to which  event 1 is forwarded*/
    const    XMC_CCU4_SLICE_SR_ID_t                 sr_e2;                    /**<Service request node to which  event 2 is forwarded*/

    const    XMC_CCU4_SLICE_EVENT_CONFIG_t *const     event0_config_ptr;      /**<Points to the variable containing event 0 configuration*/
    const    XMC_CCU4_SLICE_EVENT_CONFIG_t *const     event1_config_ptr;      /**<Points to the variable containing event 1 configuration*/
    const    XMC_CCU4_SLICE_EVENT_CONFIG_t *const     event2_config_ptr;      /**<Points to the variable containing event 2 configuration*/

    const    XMC_CCU4_SLICE_EVENT_t                   ext_start_event;        /**<Defines to which event external start signal is connected*/
    const    XMC_CCU4_SLICE_START_MODE_t              ext_start_mode;         /**<Defines mode of starting the timer*/

    const    XMC_CCU4_SLICE_EVENT_t                   ext_stop_event;         /**<Defines to which event external stop signal is connected*/
    const    XMC_CCU4_SLICE_END_MODE_t                ext_stop_mode;          /**<Defines mode of stopping the timer*/

    const    XMC_CCU4_SLICE_EVENT_t                   ext_count_dir_event;    /**<Defines to which event external count direction signal is connected*/

    const    XMC_CCU4_SLICE_EVENT_t                   ext_gate_event;         /**<Defines to which event external gating signal is connected*/

    const    XMC_CCU4_SLICE_EVENT_t                   ext_count_event;        /**<Defines to which event external count signal is connected*/

    const    XMC_CCU4_SLICE_EVENT_t                   ext_load_event;         /**<Defines to which event external load signal is connected*/

    const    XMC_CCU4_SLICE_EVENT_t                   ext_mod_event;          /**<Defines to which event external modulation signal is connected*/
    const    XMC_CCU4_SLICE_MODULATION_MODE_t         ext_mod_mode;           /**<Defines mode of external modulation*/
    const    bool                                     ext_mod_sync;           /**<Defines mode of synchronization for external modulation*/

    const    XMC_CCU4_SLICE_EVENT_t                   ext_override_edge_event;/**<Defines to which event external edge override signal is connected*/

    const    XMC_CCU4_SLICE_EVENT_t                   ext_override_level_event;/**<Defines to which event external level override signal is connected*/

    const    bool                                     ext_trap_enable;         /**<Enables the trap*/
    const    XMC_CCU4_SLICE_EVENT_t                   ext_trap_event;          /**<Defines to which event external trap signal is connected*/
    const    bool                                     ext_trap_sync;           /**<Defines mode of synchronization*/
    const    XMC_CCU4_SLICE_TRAP_EXIT_MODE_t          ext_trap_exit;           /**<Defines mode of exiting trap state*/

    const    XMC_CCU4_MULTI_CHANNEL_SHADOW_TRANSFER_t mcm_shadow_txfr_mode;    /**<Defines the mode of shadow transfer in multi channel mode operation*/

#if (UC_SERIES == XMC14)/*below feature available in XMC14xx devices */
    const    XMC_CCU4_SLICE_SHADOW_TRANSFER_MODE_t    shadow_transfer_mode;        /**<Defines the timer value(s) at which shadow transfer trigger is generated*/
    const    uint32_t                                 immediate_write;             /**<Defines the registers that are enabled for immediate shadow transfer*/
    const    uint32_t                                 automatic_shadow_transfer;   /**<Defines the registers that are enabled for automatic shadow transfer*/
    const    bool                                     cascaded_shadow_txfr_enable; /**<Enables cascade of shadow transfer in timer concatenate mode*/
#endif

    const    XMC_CCU4_SLICE_COMPARE_CONFIG_t *const ccu4_cc4_slice_timer_ptr;  /**<Points to the variable CCU4 timer initialization*/

    const    bool                                   gpio_ch_out_enable;        /**<Enables GPIO initialization for channel 1 direct output*/
             XMC_GPIO_PORT_t   *const               gpio_ch_out_ptr;           /**<Points to PORT BASE address*/
    const    uint8_t                                gpio_ch_out_pin;           /**<Pin number in the selected PORT*/
    const    XMC_GPIO_CONFIG_t *const               gpio_ch_out_config_ptr;    /**<Points to the variable containing GPIO configuration*/

             GLOBAL_CCU4_t      *const              global_ccu4_handle;        /**<Points to GLOBAL_CCU4 APP handle*/

} PWM_CCU4_CONFIG_t;

/**
 * @brief Initialization parameters of the PWM_CCU4 APP
 */
typedef struct PWM_CCU4_HandleType
{
  const PWM_CCU4_CONFIG_t      *const  config_ptr;                  /**<Points to the variable containing PWM_CCU4 APP configuration*/
        XMC_CCU4_MODULE_t      *const  ccu4_module_ptr;             /**<Points to CCU4 global register base address*/
        XMC_CCU4_SLICE_t       *const  ccu4_slice_ptr;              /**<Points to CCU4 slice register base address*/
  const uint8_t                        kernel_number;               /**<CCU4 Kernel number*/
  const uint8_t                        slice_number;                /**<CCU4 slice number*/
  const uint32_t                       shadow_txfr_msk;             /**<Mask for enabling shadow transfer of period and compare registers*/
  const uint32_t                       dither_shadow_txfr_msk;      /**<Mask for enabling shadow transfer of dither registers*/
  const uint32_t                       prescaler_shadow_txfr_msk;   /**<Mask for enabling shadow transfer of floating prescaler registers*/

        PWM_CCU4_STATE_t               state;                       /**<Defines the current state of the PWM_CCU4 APP*/
        uint32_t                       frequency_tclk;              /**<Defines the operating frequency of the CCU4 slice*/
        uint32_t                       sym_duty;                    /**<Defines the channel 1 duty cycle in symmetric mode*/
} PWM_CCU4_t;

/**
 * @}
 */

/**
 * @ingroup PWM_CCU4_apidoc
 * @{
 */

/***********************************************************************************************************************
 * API Prototypes
 **********************************************************************************************************************/
/* Support for C++ */
#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief Retrieves the version of the PWM_CCU4 APP.
 * @param None
 * @return DAVE_APP_VERSION_t APP version information (major, minor and patch number)
 *
 * \par<b>Description: </b><br>
 * The function can be used to check application software compatibility with a specific version of the APP.
 *
 *Example Usage:
 *
 * @code
  #include <DAVE.h>

  int main(void)
  {
    DAVE_APP_VERSION_t version;
    version = PWM_CCU4_GetAppVersion();
    while(1);
    return 0;
  }
 * @endcode
 */
DAVE_APP_VERSION_t PWM_CCU4_GetAppVersion(void);

/**
 * @brief Initializes the PWM_CCU4 APP.
 * @param  handle_ptr Pointer to PWM_CCU4_t structure containing APP parameters.
 * @return PWM_CCU4_STATUS_t
 *
 * \par<b>Description: </b><br>
 * Configures the CCU4 slice registers with the selected PWM_CCU4 parameters. The slice is configured in PWM generation mode.
 *
 * Example Usage:
 * @code
  #include <DAVE.h>
  
  int main(void)
  {
   DAVE_Init(); //PWM_CCU4_Init() is called by DAVE_Init().
    while(1);
    return 0;
  }
 * @endcode
 */

PWM_CCU4_STATUS_t PWM_CCU4_Init(PWM_CCU4_t* const handle_ptr);

/**
 * @brief Start the selected CCU4 slice.
 * @param  handle_ptr Pointer to PWM_CCU4_t structure containing APP parameters.
 * @return PWM_CCU4_STATUS_t
 *
 * \par<b>Description: </b><br>
 * Starts the selected CCU4 slice for PWM generation. Returns PWM_CCU4_STATUS_SUCCESS if the PWM_CCU4 APP state
 * is in "PWM_CCU4_STATE_INITIALIZED" or "PWM_CCU4_STOPPED" else returns PWM_CCU4_STATUS_FAILURE.
 * <br>PWM_CCU4_Start() is needed to be called if "Start during initialization" is unchecked to start PWM generation,
 * else its called by DAVE_Init();
 *
 * Example Usage:
 * @code
  #include <DAVE.h>
  
  int main(void)
  {
   DAVE_Init();
   //This API needs to be called if "Start during initialization" is unchecked
   PWM_CCU4_Start(&PWM_CCU4_0);
   while(1);
   return 0;
  }
 * @endcode
*/
  PWM_CCU4_STATUS_t PWM_CCU4_Start(PWM_CCU4_t* const handle_ptr);

  /**
   * @brief Stop the selected CCU4 slice.
   * @param  handle_ptr Pointer to PWM_CCU4_t structure containing APP parameters.
   * @return PWM_CCU4_STATUS_t
   *
   * \par<b>Description: </b><br>
   * Stops the selected CCU4 slice form PWM generation. Returns PWM_CCU4_STATUS_SUCCESS if the PWM_CCU4 APP state
   * is not "PWM_CCU4_STATE_UNINITIALIZED" else returns PWM_CCU4_STATUS_FAILURE.
   *
   * Example Usage:
   * @code
     #include <DAVE.h>
    int main(void)
    {
     DAVE_Init();
     PWM_CCU4_Stop(&PWM_CCU4_0);
     while(1);
     return 0;
    }
   * @endcode
  */
  PWM_CCU4_STATUS_t PWM_CCU4_Stop(PWM_CCU4_t* const handle_ptr);

  /**
 * @brief Returns the timer value.
 * @param  handle_ptr Pointer to PWM_CCU4_t structure containing APP parameters.
 * @return uint32_t
 *
 * \par<b>Description: </b><br>
 * Returns the timer value if the APP is initialized.
 *
 * Example Usage:
 * @code
     #include <DAVE.h>
    int main(void)
    {
     uint32_t timer ;
     DAVE_Init();
     timer = PWM_CCU4_GetTimerValue(&PWM_CCU4_0);
     while(1);
     return 0;
    }
 * @endcode
 */
  uint32_t PWM_CCU4_GetTimerValue(PWM_CCU4_t* const handle_ptr);

  /**
   * @brief Returns the timer status.
   * @param handle_ptr Pointer to PWM_CCU4_t structure containing APP parameters.
   * @return XMC_CCU4_STATUS_t
   *
   * \par<b>Description: </b><br>
   * Returns true is the timer is running else returns false.
   *
   * Example Usage:
   * @code
     #include <DAVE.h>
    int main(void)
    {
     bool status;
     DAVE_Init();
     status = PWM_CCU4_GetTimerStatus(&PWM_CCU4_0);
     while(1);
     return 0;
    }
   * @endcode
  */
  bool PWM_CCU4_GetTimerStatus(PWM_CCU4_t* const handle_ptr);

  /**
   * @brief Sets the PWM frequency.
   * @param handle_ptr Pointer to PWM_CCU4_t structure containing APP parameters.
   * @param pwm_freq_hz value in Hz (uint32_t)
   * @return PWM_CCU4_STATUS_t
   *
   * \par<b>Description: </b><br>
   * Sets the PWM frequency of PWM generation. The APP should not be in "PWM_CCU4_UNINITIALIZED" state.
   * Returns PWM_CCU4_STATUS_SUCCESS if frequency update is success.
   *
   * Example Usage:
   * @code
     #include <DAVE.h>
    int main(void)
    {
     PWM_CCU4_STATUS_t status;
     DAVE_Init();
     status = PWM_CCU4_SetFreq(&PWM_CCU4_0, 100000);
     while(1);
     return 0;
    }
   * @endcode
  */
  PWM_CCU4_STATUS_t PWM_CCU4_SetFreq(PWM_CCU4_t* const handle_ptr, uint32_t pwm_freq_hz);

  /**
   * @brief Sets the duty cycle of PWM.
   * @param handle_ptr Pointer to PWM_CCU4_t structure containing APP parameters.
   * @param duty_cycle channel duty cycle  uint32_t
   * @return PWM_CCU4_STATUS_t
   *
   * \par<b>Description: </b><br>
   * Sets the PWM duty. The APP should not be in "PWM_CCU4_UNINITIALIZED" state.
   * Duty is scaled by 100.<br> The condition [duty < 100%] should be met.<br>
   * Returns PWM_CCU4_STATUS_SUCCESS if operation update is success.
   *
   * Example Usage:
   * @code
     #include <DAVE.h>
    int main(void)
    {
     PWM_CCU4_STATUS_t status;
     DAVE_Init();
                  // sets the channel duty to 40%.
     status = PWM_CCU4_SetDutyCycle(&PWM_CCU4_0, 4000);
     while(1);
     return 0;
    }
   * @endcode
  */
  PWM_CCU4_STATUS_t PWM_CCU4_SetDutyCycle(PWM_CCU4_t* const handle_ptr, uint32_t duty_cycle);


  /**
   * @brief Sets the frequency duty cycle of PWM.
   * @param handle_ptr Pointer to PWM_CCU4_t structure containing APP parameters.
   * @param pwm_freq_hz value in Hz (uint32_t)
   * @param duty channel duty
   * @return PWM_CCU4_STATUS_t
   *
   * \par<b>Description: </b><br>
   * Sets the frequency and duty of PWM. The APP should not be in "PWM_CCU4_UNINITIALIZED" state.
   * Duty is scaled by 100. <br>The condition [duty < 100%] should be met.<br>
   * Returns PWM_CCU4_STATUS_SUCCESS if operation update is success.
   *
   * Example Usage:
   * @code
     #include <DAVE.h>
    int main(void)
    {
     PWM_CCU4_STATUS_t status;
     DAVE_Init();
                  // Sets freq = 100000, channel duty = 40%
     status = PWM_CCU4_SetFreqAndDutyCycle(&PWM_CCU4_0, 100000, 4000);
     while(1);
     return 0;
    }
   * @endcode
  */
  PWM_CCU4_STATUS_t PWM_CCU4_SetFreqAndDutyCycle(PWM_CCU4_t* const handle_ptr, uint32_t pwm_freq_hz, uint32_t duty);

  /**
   * @brief Sets the dither value for period , duty or both.
   * @param handle_ptr Pointer to PWM_CCU4_t structure containing APP parameters.
   * @param dither_period apply dither to period
   * @param dither_comp apply dither to compare
   * @param dither_value dither value
   * @return void
   *
   * \par<b>Description: </b><br>
   * Sets the dither value for period , duty or both.<br>
   * dither_value: is the dither value.<br>
   * dither_period: when true, dither is applied to period.<br>
   * dither_comp: when true, dither is applied to compare.<br>
   *
   * Example Usage:
   * @code
     #include <DAVE.h>
    int main(void)
    {
     DAVE_Init();
     PWM_CCU4_SetDither(&PWM_CCU4_0,(bool) true, (bool)true, 10);
     while(1);
     return 0;
    }
   * @endcode
  */
  void PWM_CCU4_SetDither(PWM_CCU4_t* const handle_ptr, bool dither_period, bool dither_comp, uint8_t dither_value);

  /**
   * @brief Clears the trap event.
   * @param handle_ptr Pointer to PWM_CCU4_t structure containing APP parameters.
   * @return void
   *
   * \par<b>Description: </b><br>
   * Clears the trap event provided the trap condition no longer exists.
   *
   * Example Usage:
   * @code
     #include <DAVE.h>
    int main(void)
    {
     DAVE_Init();
     PWM_CCU4_ClearTrap(&PWM_CCU4_0);
     while(1);
     return 0;
    }
   * @endcode
  */
  void PWM_CCU4_ClearTrap(PWM_CCU4_t* const handle_ptr);

  /**
   * @brief Returns the interrupt status.
   * @param handle_ptr Pointer to PWM_CCU4_t structure containing APP parameters.
   * @param pwm_interrupt interrupt ID
   * @return bool
   *
   * \par<b>Description: </b><br>
   * Returns true if the interrupt flag is set, else returns false.
   *
   * Example Usage:
   * @code
     #include <DAVE.h>
    int main(void)
    {
     bool status;
     DAVE_Init();
                    // Returns period match interrupt status.
     status = PWM_CCU4_GetInterruptStatus(&PWM_CCU4_0,XMC_CCU4_SLICE_IRQ_ID_PERIOD_MATCH);
                    // Returns channel compare match interrupt status.
     status = PWM_CCU4_GetInterruptStatus(&PWM_CCU4_0,XMC_CCU4_SLICE_IRQ_ID_COMPARE_MATCH_UP);
     while(1);
     return 0;
    }
   * @endcode
  */
  bool PWM_CCU4_GetInterruptStatus(PWM_CCU4_t* const handle_ptr, XMC_CCU4_SLICE_IRQ_ID_t pwm_interrupt);

  /**
   * @brief Acknowledges the interrupt.
   * @param handle_ptr Pointer to PWM_CCU4_t structure containing APP parameters.
   * @param pwm_interrupt interrupt ID
   * @return
   *
   * \par<b>Description: </b><br>
   * Clears the interrupt status flag, provided the interrupt condition no longer exists.
   *
   * Example Usage:
   * @code
     #include <DAVE.h>
    int main(void)
    {
     DAVE_Init();
     PWM_CCU4_ClearEvent(&PWM_CCU4_0,XMC_CCU4_SLICE_IRQ_ID_PERIOD_MATCH);
     PWM_CCU4_ClearEvent(&PWM_CCU4_0,XMC_CCU4_SLICE_IRQ_ID_COMPARE_MATCH_UP);
     while(1);
     return 0;
    }
   * @endcode
  */
  void PWM_CCU4_ClearEvent(PWM_CCU4_t* const handle_ptr, XMC_CCU4_SLICE_IRQ_ID_t pwm_interrupt);


#include "pwm_ccu4_extern.h"

/**
 * @}
 */
#ifdef __cplusplus
}
#endif

#endif /* PWM_CCU4_H_ */

