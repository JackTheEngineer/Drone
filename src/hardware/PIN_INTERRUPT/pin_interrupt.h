/**
 * @file pin_interrupt.h
 * @date 2016-03-02
 *
 * NOTE:
 * This file is generated by DAVE. Any manual modification done to this file will be lost when the code is regenerated.
 *
 * @cond
 ***********************************************************************************************************************
 * PIN_INTERRUPT v4.0.2 - The PIN_INTERRUPT APP invokes user interrupt handler in a response to rising and/or falling
 *                        edge event signal on a pin.
 *
 * Copyright (c) 2016, Infineon Technologies AG
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
 * 2015-12-03:
 *     - Initial version for DAVEv4. <BR>
 * @endcond 
 *
 */

#ifndef PIN_INTERRUPT_H
#define PIN_INTERRUPT_H

/***********************************************************************************************************************
 * HEADER FILES                                                                                                      
 **********************************************************************************************************************/
#if (UC_SERIES == XMC14)
#include <xmc_scu.h>
#endif
#include <xmc_gpio.h>
#include <xmc_eru.h>
#include "pin_interrupt_conf.h"
 /**********************************************************************************************************************
 * MACROS                                                                                                            
 **********************************************************************************************************************/
#if (!((XMC_LIB_MAJOR_VERSION == 2U) && \
      (XMC_LIB_MINOR_VERSION >= 1U) && \
      (XMC_LIB_PATCH_VERSION >= 6U)))
#error "PIN_INTERRUPT requires XMC Peripheral Library v2.1.6 or higher"
#endif

/**
 * @ingroup App_publicparam
 * @{
 */
/**
 *  @brief Initialization data structure for PIN_INTERRUPT APP
 */

/**
 * @}
 */

 /**********************************************************************************************************************
 * ENUMS
 **********************************************************************************************************************/
/**
 * @ingroup PIN_INTERRUPT_enumerations
 * @{
 */
/*
 * @brief enumeration for PIN_INTERRUPT APP
 */
typedef enum PIN_INTERRUPT_STATUS
{
  PIN_INTERRUPT_STATUS_SUCCESS = 0U,        /**<APP initialization is success */
  PIN_INTERRUPT_STATUS_FAILURE = 1U         /**<APP initialization is failure */
} PIN_INTERRUPT_STATUS_t;


/**
 * Defines trigger edge for the event generation by ETLx (Event Trigger Logic, x = [0 to 3]) unit, by getting the signal
 * from ERSx(Event request source, x = [0 to 3]) unit.
 */
typedef enum PIN_INTERRUPT_EDGE
{
  PIN_INTERRUPT_EDGE_NONE = 0U, /**< no event enabled */
  PIN_INTERRUPT_EDGE_RISING = 1U,   /**< detection of rising edge generates the event */
  PIN_INTERRUPT_EDGE_FALLING = 2U,  /**< detection of falling edge generates the event */
  PIN_INTERRUPT_EDGE_BOTH = 3U      /**< detection of either edges generates the event */
} PIN_INTERRUPT_EDGE_t;
/**
 * @}
 */

/**********************************************************************************************************************
* DATA STRUCTURES
**********************************************************************************************************************/
/**
 * @ingroup PIN_INTERRUPT_datastructures
 * @{
 */

/**
 * @brief Configuration structure for PIN_INTERRUPT APP
 */
typedef struct PIN_INTERRUPT
{
  XMC_ERU_t *eru;  /**< Mapped ERU module */
  XMC_GPIO_PORT_t *port;  /**< Mapped port number */
  XMC_GPIO_CONFIG_t gpio_config;   /**< Initializes the input pin characteristics */
  XMC_ERU_ETL_CONFIG_t etl_config;  /**< reference to ERUx_ETLy (x = [0..1], y = [0..4])
                                                      module configuration */
#if (UC_SERIES == XMC14)
  XMC_SCU_IRQCTRL_t irqctrl;  /**< selects the interrupt source for a NVIC interrupt node*/
#endif
  IRQn_Type IRQn;       /**< Mapped NVIC Node */
  uint8_t irq_priority; 	  /**< Node Interrupt Priority */
#if (UC_FAMILY == XMC4)
  uint8_t irq_subpriority;  /**< Node Interrupt SubPriority only valid for XMC4x */
#endif
  uint8_t etl; /*< ETLx channel (x = [0..3])*/
  uint8_t ogu; /*< OGUy channel (y = [0..3])*/
  uint8_t pin; /*< Mapped pin number */
  bool enable_at_init;  /**< Interrupt enable for Node at initialization*/
} PIN_INTERRUPT_t;
/**
 * @}
 */

#ifdef __cplusplus
extern "C" {
#endif
/***********************************************************************************************************************
 * API Prototypes
 **********************************************************************************************************************/
/**
 * @ingroup PIN_INTERRUPT_apidoc
 * @{
 */

/**
 * @brief Initializes a PIN_INTERRUPT APP instance
 * @param handle Pointer pointing to APP data structure. Refer @ref PIN_INTERRUPT_t for details.
 * @return
 *            PIN_INTERRUPT_STATUS_SUCCESS             : if initialization is successful\n
 *            PIN_INTERRUPT_STATUS_FAILURE             : if initialization is failed
 *
 * \par<b>Description:</b><br>
 * PIN_INTERRUPT_Init API is called during initialization of DAVE APPS. This API Initializes input pin characteristics,
 * ERU and OGU hardware module initialization, Configures NVIC node and its priority in order to generate an event.
 *
 * \par<b>Example Usage:</b><br>
 *
 * @code
 * #include <DAVE.h>
 *
 * int main(void)
 * {
 *   DAVE_STATUS_t status;
 *
 *   status = DAVE_Init();  //  PIN_INTERRUPT_Init API is called during initialization of DAVE APPS
 *   if(DAVE_STATUS_SUCCESS == status)
 *   {
 *    // user code
 *
 *     while(1)
 *     {
 *
 *     }
 *   }
 *   return (1);
 * }
 *
 * @endcode<BR>
 */
PIN_INTERRUPT_STATUS_t PIN_INTERRUPT_Init(const PIN_INTERRUPT_t *const handle);

/**
 * @brief Enables the IRQ.
 * @param handle Pointer pointing to APP data structure. Refer @ref PIN_INTERRUPT_t for details.
 * @return None
 * <BR><P ALIGN="LEFT"><B>Example:</B>
 *
 * @code
 *  #include <DAVE.h>
 *
 *  int main(void)
 *  {
 *    DAVE_STATUS_t status;
 *
 *   status = DAVE_Init();  //  PIN_INTERRUPT_Init API is called during initialization of DAVE APPS
 *   if(DAVE_STATUS_SUCCESS == status)
 *   {
 *    // user code
 *
 *    PIN_INTERRUPT_Enable(&PIN_INTERRUPT_0);
 *    while(1)
 *    {}
 *   }
 *
 *    return (1);
 *  }
 * @endcode<BR> </p>
 */
__STATIC_INLINE void PIN_INTERRUPT_Enable(const PIN_INTERRUPT_t *const handle)
{
  XMC_ASSERT("PIN_INTERRUPT_Enable: Handler null pointer", handle != NULL);
  NVIC_EnableIRQ(handle->IRQn);
}

/**
 * @brief Disables the IRQ.
 * @param handle Pointer pointing to APP data structure. Refer @ref PIN_INTERRUPT_t for details.
 * @return None
 * <BR><P ALIGN="LEFT"><B>Example:</B>
 *
 * @code
 *  #include <DAVE.h>
 *
 *  int main(void)
 *  {
 *
 *    DAVE_STATUS_t status;
 *
 *   status = DAVE_Init();  //  PIN_INTERRUPT_Init API is called during initialization of DAVE APPS
 *   if(DAVE_STATUS_SUCCESS == status)
 *   {
 *    // user code
 *
 *    PIN_INTERRUPT_Disable(&PIN_INTERRUPT_0);
 *    while(1)
 *    {}
 *   }
 *
 *    return (1);
 *
 *  }
 * @endcode<BR> </p>
 */
__STATIC_INLINE void PIN_INTERRUPT_Disable(const PIN_INTERRUPT_t *const handle)
{
  XMC_ASSERT("PIN_INTERRUPT_Disable: Handler null pointer", handle != NULL);
  NVIC_DisableIRQ(handle->IRQn);
}

/**
* @brief Read input level of port pin.
* @param handle Pointer pointing to APP data structure. Refer @ref PIN_INTERRUPT_t for details.
* @return uint32_t input logic level. Range:0-1
*
* \par<b>Description:</b><br>
* This function reads the Pn_IN register and returns the current logical value at the GPIO pin.
*
*
* Example Usage:
* @code
* #include <DAVE.h>//Declarations from DAVE Code Generation (includes SFR declaration)
* int main(void)
* {
*   DAVE_STATUS_t status;
*   uint32_t pin_status;
*   status = DAVE_Init();   // (DAVE_STATUS_t)PIN_INTERRUPT_Init(&PIN_INTERRUPT_0) is called within DAVE_Init()
*   if(status == DAVE_STATUS_SUCCESS)
*   {
*     XMC_DEBUG("DAVE Apps initialization success\n");
*   }
*   else
*   {
*     XMC_DEBUG(("DAVE Apps initialization failed with status %d\n", status));
*     while(1U)
*     {
*     }
*   }
*   //Placeholder for user application code. The while loop below can be replaced with user application code.
*   while(1U)
*   {
*     pin_status = PIN_INTERRUPT_GetPinValue(&PIN_INTERRUPT_0);
*     if(pin_status == 1)
*     {
*       // Add application code here
*     }
*     else
*     {
*       // Add application code here
*     }
*   }
*   return (1);
* }
*  @endcode
*/
__STATIC_INLINE uint32_t PIN_INTERRUPT_GetPinValue(const PIN_INTERRUPT_t *const handle)
{
  XMC_ASSERT("PIN_INTERRUPT_GetPinValue: Handler null pointer", handle != NULL);
  return (XMC_GPIO_GetInput(handle->port, handle->pin));
}

/**
* @brief Function to configure event trigger edge.
* @param handle Pointer pointing to APP data structure. Refer @ref PIN_INTERRUPT_t and @ref PIN_INTERRUPT_EDGE_t
* for more details.
* @param edge event trigger edge.
* @return none
*
* \par<b>Description:</b><br>
* Configures event trigger edge during run time. Rising edge, falling edge or both edges can be selected to generate
* the event.
*
*
* Example Usage:
* @code
* #include <DAVE.h>//Declarations from DAVE Code Generation (includes SFR declaration)
* int main(void)
* {
*   DAVE_STATUS_t status;
*   status = DAVE_Init();   // (DAVE_STATUS_t)PIN_INTERRUPT_Init(&PIN_INTERRUPT_0) is called within DAVE_Init()
*   if(status == DAVE_STATUS_SUCCESS)
*   {
*     XMC_DEBUG("DAVE Apps initialization success\n");
*   }
*   else
*   {
*     XMC_DEBUG(("DAVE Apps initialization failed with status %d\n", status));
*     while(1U)
*     {
*     }
*   }
*   // Configure rising edge as event trigger edge
*   PIN_INTERRUPT_SetEdgeSensitivity(&PIN_INTERRUPT_0, PIN_INTERRUPT_EDGE_RISING);
*
*
*   //Placeholder for user application code. The while loop below can be replaced with user application code.
*   while(1U)
*   {
*
*       // Add application code here
*
*   }
*   return (1);
* }
*  @endcode
*/
void PIN_INTERRUPT_SetEdgeSensitivity(const PIN_INTERRUPT_t *const handle, const PIN_INTERRUPT_EDGE_t edge);

/**
* @brief Function to get the configured event trigger edge.
* @param handle Pointer pointing to APP data structure. Refer @ref PIN_INTERRUPT_t and @ref PIN_INTERRUPT_EDGE_t
* for more details.
* @return PIN_INTERRUPT_EDGE_t configured event trigger edge
*
* \par<b>Description:</b><br>
* Get the configured event trigger edge during run time.
*
*
* Example Usage:
* @code
* #include <DAVE.h>//Declarations from DAVE Code Generation (includes SFR declaration)
* int main(void)
* {
*   DAVE_STATUS_t status;
*   status = DAVE_Init();   // (DAVE_STATUS_t)PIN_INTERRUPT_Init(&PIN_INTERRUPT_0) is called within DAVE_Init()
*   PIN_INTERRUPT_EDGE_t edge_val;
*   if(status == DAVE_STATUS_SUCCESS)
*   {
*     XMC_DEBUG("DAVE Apps initialization success\n");
*   }
*   else
*   {
*     XMC_DEBUG(("DAVE Apps initialization failed with status %d\n", status));
*     while(1U)
*     {
*     }
*   }
*   // Configure rising edge as event trigger edge
*   edge_val = PIN_INTERRUPT_GetEdgeSensitivity(&PIN_INTERRUPT_0);
*   // Validate edge_val against expected event trigger edge
*
*
*   //Placeholder for user application code. The while loop below can be replaced with user application code.
*   while(1U)
*   {
*
*       // Add application code here
*
*   }
*   return (1);
* }
*  @endcode
*/
PIN_INTERRUPT_EDGE_t PIN_INTERRUPT_GetEdgeSensitivity(const PIN_INTERRUPT_t *const handle);

/**
 * @}
 */
#ifdef __cplusplus
}
#endif   

extern const PIN_INTERRUPT_t RFM75_IRQ_PIN;
#endif /* End of PIN_INTERRUPT_H */
