/*******************************************************************************
 Copyright (c) 2015, Infineon Technologies AG                                 **
 All rights reserved.                                                         **
                                                                              **
 Redistribution and use in source and binary forms, with or without           **
 modification,are permitted provided that the following conditions are met:   **
                                                                              **
 *Redistributions of source code must retain the above copyright notice,      **
 this list of conditions and the following disclaimer.                        **
 *Redistributions in binary form must reproduce the above copyright notice,   **
 this list of conditions and the following disclaimer in the documentation    **
 and/or other materials provided with the distribution.                       **
 *Neither the name of the copyright holders nor the names of its contributors **
 may be used to endorse or promote products derived from this software without**
 specific prior written permission.                                           **
                                                                              **
 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"  **
 AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE    **
 IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE   **
 ARE  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE   **
 LIABLE  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR         **
 CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF         **
 SUBSTITUTE GOODS OR  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS    **
 INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN      **
 CONTRACT, STRICT LIABILITY,OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)       **
 ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE   **
 POSSIBILITY OF SUCH DAMAGE.                                                  **
                                                                              **
 To improve the quality of the software, users are encouraged to share        **
 modifications, enhancements or bug fixes with Infineon Technologies AG       **
 dave@infineon.com).                                                          **
                                                                              **
********************************************************************************
**                                                                            **
**                                                                            **
** PLATFORM : Infineon XMC4500 Series                                         **
**                                                                            **
** AUTHOR : Application Engineering Team                                      **
**                                                                            **
** version 4.0.0 (Initial version)			                                  **
**         4.1.2  Modified for APP updates                                    **
**         4.1.4  Modified for APP updates                                    **
**                                                                            **
** MODIFICATION DATE : October, 30, 2015                                      **
**                                                                            **
*******************************************************************************/

#include <DAVE.h>			//Declarations from DAVE Code Generation (includes SFR declarations)
#include <xmc_ccu4.h>		//CCU4 LLD

volatile uint32_t interruptcount=0;

/* Pre-calculated prescaler, period value for 0.1 second */
uint8_t newprescaler_0_1second = 8;
uint16_t newperiod_0_1second = 46875;

/**
 * @brief main() - Application entry point
 * No code in the main routine. All update made in the ISR handler.
 */
int main(void)
{
  DAVE_STATUS_t status;

  status = DAVE_Init();		/* Initialization of DAVE APPs  */

  if(status == DAVE_STATUS_FAILURE)
  {
    /* Placeholder for error handler code. The while loop below can be replaced with an user error handler */
    XMC_DEBUG(("DAVE APPs initialization failed with status %d\n", status));
    while(1U)
    {
    }
  }

  /* Placeholder for user application code. The while loop below can be replaced with user application code. */
  while(1U)
  {
  }

  return 1;
}

/**
 * @brief periodmatchhandler() - Period match interrupt routine
 * This routine updates the PWM with a new frequency every 10 seconds
 */

void periodmatchhandler(void)
{
	/* Acknowledge Period Match Interrupt */
	PWM_ClearEvent(&PWM_0, PWM_INTERRUPT_PERIODMATCH);

	interruptcount++;

	if(interruptcount==10)
	{
		/* Using the CCU4 LLD, update the CRS, PRS, PSC for 0.1 second period*/
		XMC_CCU4_SLICE_SetPrescaler(PWM_0.ccu4_slice_ptr,newprescaler_0_1second);
		XMC_CCU4_SLICE_SetTimerPeriodMatch(PWM_0.ccu4_slice_ptr,newperiod_0_1second);
		XMC_CCU4_SLICE_SetTimerCompareMatch(PWM_0.ccu4_slice_ptr,newperiod_0_1second/2);
		XMC_CCU4_EnableShadowTransfer(PWM_0.ccu4_kernel_ptr,
	                                   (uint32_t)(((uint32_t)XMC_CCU4_SHADOW_TRANSFER_SLICE_0 |
	                        		   (uint32_t)XMC_CCU4_SHADOW_TRANSFER_PRESCALER_SLICE_0) <<
		                    	       (uint32_t)((uint32_t)PWM_0.slice_number << (uint32_t)2)));


	}
	else if(interruptcount==110)
	{
		/* Set frequency to 1 Hz*/
		PWM_SetFreq(&PWM_0, 1);

		/* Resets the count*/
		interruptcount=0;
	}

}
