/**************************************************************/
/* Author: Jakov Kholodkov				      */
/* Code parly copied from Infineon Larix Quadrocopter Project */
/**************************************************************/

#include "I2Cdev.h"

const XMC_I2C_CH_CONFIG_t i2c_channel_config =
{
	.baudrate = (uint32_t)(400000U),
	.address  = 0
};

const XMC_GPIO_CONFIG_t i2c_sda_pin_config   =
{
	.mode = XMC_GPIO_MODE_OUTPUT_OPEN_DRAIN_ALT1,
	.output_level   = XMC_GPIO_OUTPUT_LEVEL_HIGH,
	.output_strength = XMC_GPIO_OUTPUT_STRENGTH_STRONG_SOFT_EDGE,
};
const XMC_GPIO_CONFIG_t i2c_scl_pin_config   =
{
	.mode = XMC_GPIO_MODE_OUTPUT_OPEN_DRAIN_ALT1,
	.output_level  = XMC_GPIO_OUTPUT_LEVEL_HIGH,
	.output_strength = XMC_GPIO_OUTPUT_STRENGTH_STRONG_SOFT_EDGE,
};

void setupI2CInterface(XMC_USIC_CH_t *const i2c_usic)
{
	XMC_I2C_CH_Init(i2c_usic, &i2c_channel_config);
	/*******************************************************/
	/* The number (3rd argument) stands for		       */
	/* 	           | Number | DXn    |		       */
	/* 		   |--------+--------|		       */
	/* 		   |      0 | DXnA   |		       */
	/* 		   |      1 | DXnB   |		       */
	/* 		   |      2 | DXnC   |		       */
	/* 		   |      3 | DXnD   |		       */
	/* 		   |      4 | DXnE   |		       */
	/* 		   |      5 | DXnF   |		       */
	/* 		   |      6 | DXnG   |		       */
	/* 		   |      7 | Always |		       */
	/* look at page 1772 (miko-keil manual xmc4000)	       */
	/*     for the usic interconnect		       */
	/*******************************************************/
	/* This activates the connection between the DX1 signal and the 5.1 / 5.2 pin */
	XMC_I2C_CH_SetInputSource(i2c_usic, XMC_USIC_CH_INPUT_DX1, 0); /* Clock Input select  P5.2 */
	XMC_I2C_CH_SetInputSource(i2c_usic, XMC_USIC_CH_INPUT_DX0, 0); /* Clock Input select  P5.1 */
	
	XMC_USIC_CH_RXFIFO_Configure(i2c_usic, 0, XMC_USIC_CH_FIFO_SIZE_32WORDS, 0);
	XMC_I2C_CH_Start(i2c_usic);
	
	XMC_GPIO_Init((XMC_GPIO_PORT_t *)PORT5_BASE, (uint8_t)1, &i2c_sda_pin_config);
	XMC_GPIO_Init((XMC_GPIO_PORT_t *)PORT5_BASE, (uint8_t)2, &i2c_scl_pin_config);
}

bool I2Cdev_writeByte(XMC_USIC_CH_t* i2c_usic,uint8_t devAddr, uint8_t regAddr, uint8_t data)
{
	uint16_t time_out_cnt = 0u;
	XMC_I2C_CH_MasterStart(i2c_usic, devAddr, XMC_I2C_CH_CMD_WRITE);
	while ((XMC_I2C_CH_GetStatusFlag(i2c_usic) & XMC_I2C_CH_STATUS_FLAG_ACK_RECEIVED) == 0U)
	{
		time_out_cnt++;
		if (time_out_cnt > 200u)
		{
			return false;
		}
	}
	XMC_I2C_CH_ClearStatusFlag(i2c_usic,XMC_I2C_CH_STATUS_FLAG_ACK_RECEIVED);

	time_out_cnt = 0u;
	XMC_I2C_CH_MasterTransmit(i2c_usic, regAddr);
	while ((XMC_I2C_CH_GetStatusFlag(i2c_usic) & XMC_I2C_CH_STATUS_FLAG_ACK_RECEIVED) == 0U)
	{
		time_out_cnt++;
		if (time_out_cnt > 200u)
		{
			return false;
		}
	}
	XMC_I2C_CH_ClearStatusFlag(i2c_usic,XMC_I2C_CH_STATUS_FLAG_ACK_RECEIVED);

	time_out_cnt = 0u;
	XMC_I2C_CH_MasterTransmit(i2c_usic, data);
	while ((XMC_I2C_CH_GetStatusFlag(i2c_usic) & XMC_I2C_CH_STATUS_FLAG_ACK_RECEIVED) == 0U)
	{
		time_out_cnt++;
		if (time_out_cnt > 200u)
		{
			return false;
		}
	}
	XMC_I2C_CH_ClearStatusFlag(i2c_usic,XMC_I2C_CH_STATUS_FLAG_ACK_RECEIVED);

	XMC_I2C_CH_MasterStop(i2c_usic);
	return true;
}


int16_t I2Ctest(XMC_USIC_CH_t* i2c_usic,
			uint8_t devAddr,
			uint8_t regAddr){

	XMC_I2C_CH_MasterStart(i2c_usic, devAddr, XMC_I2C_CH_CMD_WRITE);

}

int16_t I2Cdev_readByte(XMC_USIC_CH_t* i2c_usic,
			uint8_t devAddr,
			uint8_t regAddr)
{
	uint16_t time_out_cnt = 0u;
	XMC_I2C_CH_MasterStart(i2c_usic, devAddr, XMC_I2C_CH_CMD_WRITE);
	while ((XMC_I2C_CH_GetStatusFlag(i2c_usic) & XMC_I2C_CH_STATUS_FLAG_ACK_RECEIVED) == 0U)
	{
		time_out_cnt++;
		if (time_out_cnt > 300u)
		{
			return -1;
		}
	}
	XMC_I2C_CH_ClearStatusFlag(i2c_usic,XMC_I2C_CH_STATUS_FLAG_ACK_RECEIVED);

	time_out_cnt = 0u;
	XMC_I2C_CH_MasterTransmit(i2c_usic, regAddr);
	XMC_I2C_CH_MasterRepeatedStart(i2c_usic, devAddr,XMC_I2C_CH_CMD_READ);
	while ((XMC_I2C_CH_GetStatusFlag(i2c_usic) & XMC_I2C_CH_STATUS_FLAG_ACK_RECEIVED) == 0U)
	{
		time_out_cnt++;
		if (time_out_cnt > 200u)
		{
			return -1;
		}
	}
	XMC_I2C_CH_ClearStatusFlag(i2c_usic,XMC_I2C_CH_STATUS_FLAG_ACK_RECEIVED);

	time_out_cnt = 0u;
	XMC_I2C_CH_MasterReceiveNack(i2c_usic);
	while ((XMC_I2C_CH_GetStatusFlag(i2c_usic) &
		(XMC_I2C_CH_STATUS_FLAG_RECEIVE_INDICATION |
		 XMC_I2C_CH_STATUS_FLAG_ALTERNATIVE_RECEIVE_INDICATION)) == 0U)
	{
		time_out_cnt++;
		if (time_out_cnt > 200u)
		{
			return -1;
		}
	}
	XMC_I2C_CH_ClearStatusFlag(i2c_usic,XMC_I2C_CH_STATUS_FLAG_RECEIVE_INDICATION | XMC_I2C_CH_STATUS_FLAG_ALTERNATIVE_RECEIVE_INDICATION);

	uint8_t received_data = 0;
	received_data = XMC_I2C_CH_GetReceivedData(i2c_usic);

	XMC_I2C_CH_MasterStop(i2c_usic);

	return (int16_t)received_data;
}

int16_t I2Cdev_readBytes(XMC_USIC_CH_t* i2c_usic,
			 uint8_t devAddr,
			 uint8_t regAddr,
			 uint8_t length,
			 uint8_t* data)
{
	uint16_t time_out_cnt = 0u;
	XMC_I2C_CH_MasterStart(i2c_usic, devAddr, XMC_I2C_CH_CMD_WRITE);
	while ((XMC_I2C_CH_GetStatusFlag(i2c_usic) & XMC_I2C_CH_STATUS_FLAG_ACK_RECEIVED) == 0U)
	{
		time_out_cnt++;
		if (time_out_cnt > 200u)
		{
			return -1;
		}
	}
	XMC_I2C_CH_ClearStatusFlag(i2c_usic,
				   XMC_I2C_CH_STATUS_FLAG_ACK_RECEIVED);

	time_out_cnt = 0u;
	XMC_I2C_CH_MasterTransmit(i2c_usic, regAddr);
	while ((XMC_I2C_CH_GetStatusFlag(i2c_usic) & XMC_I2C_CH_STATUS_FLAG_ACK_RECEIVED) == 0U)
	{
		time_out_cnt++;
		if (time_out_cnt > 200u)
		{
			return -1;
		}
	}
	XMC_I2C_CH_ClearStatusFlag(i2c_usic,XMC_I2C_CH_STATUS_FLAG_ACK_RECEIVED);

	time_out_cnt = 0u;
	XMC_I2C_CH_MasterRepeatedStart(i2c_usic, devAddr,XMC_I2C_CH_CMD_READ);
	while ((XMC_I2C_CH_GetStatusFlag(i2c_usic) & XMC_I2C_CH_STATUS_FLAG_ACK_RECEIVED) == 0U)
	{
		time_out_cnt++;
		if (time_out_cnt > 200u)
		{
			return -1;
		}
	}
	XMC_I2C_CH_ClearStatusFlag(i2c_usic,XMC_I2C_CH_STATUS_FLAG_ACK_RECEIVED);

	int16_t fifo_bytes;
	for(fifo_bytes = 0; fifo_bytes < length; fifo_bytes++)
	{
		if(fifo_bytes < (length-1))
			XMC_I2C_CH_MasterReceiveAck(i2c_usic);
		else
			XMC_I2C_CH_MasterReceiveNack(i2c_usic);
			time_out_cnt = 0u;
			while ((XMC_I2C_CH_GetStatusFlag(i2c_usic) &
				(XMC_I2C_CH_STATUS_FLAG_RECEIVE_INDICATION |
				 XMC_I2C_CH_STATUS_FLAG_ALTERNATIVE_RECEIVE_INDICATION)) == 0U)
			{
				time_out_cnt++;
				if (time_out_cnt > 200u)
				{
					return -1;
				}
			}
			XMC_I2C_CH_ClearStatusFlag(i2c_usic,
						   XMC_I2C_CH_STATUS_FLAG_RECEIVE_INDICATION |
						   XMC_I2C_CH_STATUS_FLAG_ALTERNATIVE_RECEIVE_INDICATION);
	}
	
	XMC_I2C_CH_MasterStop(i2c_usic);
	
	fifo_bytes=0;
	while(!XMC_USIC_CH_RXFIFO_IsEmpty(i2c_usic))
	{
		data[fifo_bytes] = XMC_I2C_CH_GetReceivedData(i2c_usic);
		fifo_bytes++;
	}

	XMC_I2C_CH_ClearStatusFlag(i2c_usic,
				   XMC_I2C_CH_STATUS_FLAG_RECEIVE_INDICATION |
				   XMC_I2C_CH_STATUS_FLAG_ALTERNATIVE_RECEIVE_INDICATION);
	return fifo_bytes;
}

