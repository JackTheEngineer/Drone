#include "spi_wrapper.h"

void SPI_transmit(const SPI_MASTER_t *const handle, uint8_t* data, uint32_t count){
	uint32_t status1;
	uint32_t status2;
	SPI_MASTER_SetMode(&SPI_MASTER_0, XMC_SPI_CH_MODE_STANDARD);
	XMC_USIC_CH_SetFrameLength(XMC_SPI1_CH1,8);
	SPI_MASTER_EnableSlaveSelectSignal(&SPI_MASTER_0, SPI_MASTER_0.config->slave_select_pin_config[0]->slave_select_ch);
	
	/* Clear the flags */
	SPI_MASTER_ClearFlag(&SPI_MASTER_0, XMC_SPI_CH_STATUS_FLAG_ALTERNATIVE_RECEIVE_INDICATION);
	SPI_MASTER_ClearFlag(&SPI_MASTER_0, XMC_SPI_CH_STATUS_FLAG_RECEIVE_INDICATION);
	  
	_SPI_transmit(&SPI_MASTER_0, data, count);
	while(SPI_MASTER_0.runtime->tx_busy);
	/* Wait till dummy data is received from flash chip */
	do
	{
		status1 = SPI_MASTER_GetFlagStatus(&SPI_MASTER_0,XMC_SPI_CH_STATUS_FLAG_ALTERNATIVE_RECEIVE_INDICATION);
		status2 = SPI_MASTER_GetFlagStatus(&SPI_MASTER_0,XMC_SPI_CH_STATUS_FLAG_RECEIVE_INDICATION);
	} while(((status1 == 0) && (status2 == 0)));

	/* Clear the flags */
	SPI_MASTER_ClearFlag(&SPI_MASTER_0,XMC_SPI_CH_STATUS_FLAG_ALTERNATIVE_RECEIVE_INDICATION);
	SPI_MASTER_ClearFlag(&SPI_MASTER_0,XMC_SPI_CH_STATUS_FLAG_RECEIVE_INDICATION);
}

void _SPI_transmit(const SPI_MASTER_t *const handle, uint8_t* data, uint32_t count){
	SPI_MASTER_lStartTransmitPolling(handle, data, count);
}
