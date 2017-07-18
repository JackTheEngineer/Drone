/*********************************************************************************************************************
* DAVE APP Name : CLOCK_XMC4       APP Version: 4.0.22
*
* NOTE:
* This file is generated by DAVE. Any manual modification done to this file will be lost when the code is regenerated.
*********************************************************************************************************************/

/**
 * @cond
 ***********************************************************************************************************************
 *
 * Copyright (c) 2015-2016, Infineon Technologies AG
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,are permitted provided that the
 * following conditions are met:
 *
 *   Redistributions of source code must retain the above copyright notice, this list of conditions and the  following
 *   disclaimer.
 *
 *   Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the
 *   following disclaimer in the documentation and/or other materials provided with the distribution.
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
 * 2015-02-16:
 *     - Initial version for DAVEv4. <BR>
 * 2015-05-08:
 *     - No functional change; Updated to adhere latest guidelines <br>
 * 2015-10-08:
 *     - Ensured Trap status bit clearing for XMC48 and XMC47 device.<br>
 *     - USB PLL Configuration made based on user configuration.<br>
 *     - ECAT Configuration supported for XMC48 device.<br>
 * 2015-10-20:
 *     - High Precision Oscillator Settings are enabled only when it is required.<br>
 * 2015-12-01:
 *     - Ensured Trap status bit clearing for XMC43 device.<br>
 * 2015-12-20:
 *     - Disabled option supported in combo box for High Precision Oscillator Settings.<br>
 * 2015-12-24:
 *     - TRAP event settings are moved to SystemCoreClockSetup API from CLOCK_XMC4_Init. <BR>
 *     - Code size is improved. <BR>
 *     - OSCHP_GetFrequency API is made to available to user only when high precision oscillator is used. <BR>
 * 2016-01-08:
 *     - Removed clear trap status settings at the end of the clock initialization which was redundant code.<br>
 * 2016-05-25:
 *     - Ensured main PLL and USBPLL dependencies based on its checkbox enable via CLOCK_XMC4 APP GUI.<br>
 * 2016-07-08:
 *     - Fixed incorrect case for an included header.<br>
 *
 * @endcond
 *
 */
#include "xmc_scu.h"
 /**********************************************************************************************************************
 * API IMPLEMENTATION
 **********************************************************************************************************************/
/**
* @brief  Function to initialize the Clock Tree based on UI configuration
* @note   -
* @param  None
* @retval None
*/
void SystemCore_ClockSetup(void)
{
  /* Local data structure for initializing the clock functional block */
  const XMC_SCU_CLOCK_CONFIG_t CLOCK_XMC4_0_CONFIG =
  {
    /* N-Divider Value */
    .syspll_config.n_div = 80U,
    /* P-Divider Value */
    .syspll_config.p_div = 2U,
    /* K2-Divider Value */
    .syspll_config.k_div = 4U,
    /* PLL Operating Mode */
    .syspll_config.mode = XMC_SCU_CLOCK_SYSPLL_MODE_NORMAL,
    /* PLL Clock Source */
    .syspll_config.clksrc = XMC_SCU_CLOCK_SYSPLLCLKSRC_OSCHP,
    /* High Precision Oscillator Operating Mode */
    .enable_oschp = true,
    /* Ultra Low Power Oscillator Setting */
    .enable_osculp = false,
    /* Calibration Mode */
    .calibration_mode = XMC_SCU_CLOCK_FOFI_CALIBRATION_MODE_FACTORY,
    /* Standby Clock Source */
    .fstdby_clksrc = XMC_SCU_HIB_STDBYCLKSRC_OSI,
    /* System Clock Source */
    .fsys_clksrc = XMC_SCU_CLOCK_SYSCLKSRC_PLL,
    /* System Clock Divider Value */
    .fsys_clkdiv = 1U,
    /* CPU Clock Divider Value */
    .fcpu_clkdiv = 1U,
#ifdef CLOCK_XMC4_CCUCLK_ENABLED  
    /* CCU Clock Divider Value */
    .fccu_clkdiv = 1U,
#endif
    /* Peripheral Clock Divider Value */
    .fperipheral_clkdiv = 1U
  };
  /* Initialize the SCU clock */
  XMC_SCU_CLOCK_Init(&CLOCK_XMC4_0_CONFIG);
  /* RTC source clock */
  XMC_SCU_HIB_SetRtcClockSource(XMC_SCU_HIB_RTCCLKSRC_OSI);
  
#ifdef CLOCK_XMC4_USBCLK_ENABLED  
  /* USB/SDMMC source clock */
  XMC_SCU_CLOCK_SetUsbClockSource(XMC_SCU_CLOCK_USBCLKSRC_USBPLL);
  /* USB/SDMMC divider setting */
  XMC_SCU_CLOCK_SetUsbClockDivider(4U);
#endif
  /* Start USB PLL */
  XMC_SCU_CLOCK_StartUsbPll(1U, 32U);

#ifdef CLOCK_XMC4_WDTCLK_ENABLED    
  /* WDT source clock */
  XMC_SCU_CLOCK_SetWdtClockSource(XMC_SCU_CLOCK_WDTCLKSRC_OFI);
  /* WDT divider setting */
  XMC_SCU_CLOCK_SetWdtClockDivider(1U);
#endif

#ifdef CLOCK_XMC4_EBUCLK_ENABLED 
  /* EBU divider setting */
  XMC_SCU_CLOCK_SetEbuClockDivider(1U);
#endif

}

