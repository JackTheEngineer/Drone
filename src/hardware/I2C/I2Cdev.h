/*******************************************************************************
**                      Author(s) Identity                                    **
********************************************************************************
** Jakov Kholodkov, Code copied from larix flight-controller
**                                                                            **
*******************************************************************************/

/*******************************************************************************
**                      Revision Control History                              **
*******************************************************************************/

#ifndef I2C_E_H_
#define I2C_E_H_

#include <xmc_usic.h>
#include <xmc_i2c.h>
#include <xmc_gpio.h>

void setupI2CInterface(XMC_USIC_CH_t *const i2c_channel);
bool I2Cdev_writeByte(XMC_USIC_CH_t* handle,
		      uint8_t devAddr,
		      uint8_t regAddr,
		      uint8_t data);
int16_t I2Cdev_readByte (XMC_USIC_CH_t* handle,
			 uint8_t devAddr,
			 uint8_t regAddr);
int16_t I2Cdev_readBytes(XMC_USIC_CH_t* handle,
			 uint8_t devAddr,
			 uint8_t regAddr,
			 uint8_t length,
			 uint8_t* data);

#endif /* I2C_E_H_ */
