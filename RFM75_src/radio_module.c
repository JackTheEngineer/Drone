#include "base.h"
#include "RFM75_driver.h"
#include "radio_module.h"
#include "delay_us.h"
#include "RFM75_codes.h"

void RFM75_set_RX_mode(void);
bool RFM75_initRegisters(void);

_STATIC_ volatile uint8_t rxtx_interrupt;

const uint8_t RFM75_cmd_adrRX0[] = { WRITE_COMMAND_RFM(0x0A), 0xAA, 0xAA, 0xAA, 0xAA, 0xAA};
const uint8_t RFM75_cmd_adrTX[]  = { WRITE_COMMAND_RFM(0x10), 0xAA, 0xAA, 0xAA, 0xAA, 0xAA};
const uint8_t RFM75_cmd_adrRX1[] = { WRITE_COMMAND_RFM(0x0B), 0x35, 0x43, 0x10, 0x10, 0x02};
const uint8_t RFM75_cmd_switch_bank[] = { 0x50, 0x53 }; // 
const uint8_t RFM75_cmd_flush_rx[] = { 0xe2, 0x00 }; // flush RX FIFO
const uint8_t RFM75_cmd_flush_tx[] = { 0xe1, 0x00 }; // flush TX FIFO
const uint8_t RFM75_cmd_activate[] = { 0x50, 0x73 }; // 
const uint8_t RFM75_cmd_tog1[] = { WRITE_COMMAND_RFM(0x04), 0xF9 | 0x06, 0x96, 0x82, 0xDB };
const uint8_t RFM75_cmd_tog2[] = { WRITE_COMMAND_RFM(0x04), 0xF9, 0x96, 0x82, 0xDB};

//************ Bank0 register initialization commands
const uint8_t  RFM75_Bank0[][2] = {
		// address data
		{ WRITE_COMMAND_RFM(CONFIG_REG), 0x0F }, //CRC=1byte, POWER UP, RX
		{ WRITE_COMMAND_RFM(ENABLE_AUTO_ACK_REG), ALL_6_PIPES },
		{ WRITE_COMMAND_RFM(ENABLE_RX_ADDR_REG), ALL_6_PIPES },
		{ WRITE_COMMAND_RFM(SETUP_ADDR_WIDTH), ADDRESS_WIDTH_5 },
		// AUTO_RETRANSMISSION_DELAY is given in multiples of 250 uS
		{ WRITE_COMMAND_RFM(SETUP_RETR), ( AUTO_RETR_DELAY(4) | \
												AUTO_RETR_COUNT(15)) }, // 1 ms retr. delay
		{ WRITE_COMMAND_RFM(RF_CH_REG), RF_CHANNEL(0x17) },
		{ WRITE_COMMAND_RFM(RF_SETTINGS_REG), 0x0F },
		{ WRITE_COMMAND_RFM(STATUS_REG), 0x07 },
		{ WRITE_COMMAND_RFM(OBSERVE_TX_REG), 0x00 },
		{ WRITE_COMMAND_RFM(CARRIER_DETECT_REG), 0x00 },
		{ WRITE_COMMAND_RFM(RX_PIPE_2_ADDR_REG), 0xc3 },
		{ WRITE_COMMAND_RFM(RX_PIPE_3_ADDR_REG), 0xc4 },
		{ WRITE_COMMAND_RFM(RX_PIPE_4_ADDR_REG), 0xc5 },
		{ WRITE_COMMAND_RFM(RX_PIPE_5_ADDR_REG), 0xc6 },
		{ WRITE_COMMAND_RFM(RX_PIPE_0_PAYLOAD_LENGTH_REG), 32 },
		{ WRITE_COMMAND_RFM(RX_PIPE_1_PAYLOAD_LENGTH_REG), 32 },
		{ WRITE_COMMAND_RFM(RX_PIPE_2_PAYLOAD_LENGTH_REG), 32 },
		{ WRITE_COMMAND_RFM(RX_PIPE_3_PAYLOAD_LENGTH_REG), 32 },
		{ WRITE_COMMAND_RFM(RX_PIPE_4_PAYLOAD_LENGTH_REG), 32 },
		{ WRITE_COMMAND_RFM(RX_PIPE_5_PAYLOAD_LENGTH_REG), 32 },
		{ WRITE_COMMAND_RFM(FIFO_STATUS_REG), 0x00 },
		{ WRITE_COMMAND_RFM(DYNAMIC_PAYLOAD_LENGTH_REG), ALL_6_PIPES},
		{ WRITE_COMMAND_RFM(FEATURE_REG), (EN_DYNAMIC_PAYLOAD_LENGTH | \
												EN_PAYLOAD_WITH_ACK | \
												EN_SPECIAL_NOACK_COMMAND) }
};

//************ Bank1 register initialization commands
const uint8_t RFM75_Bank1[][5] = {
		{ WRITE_COMMAND_RFM(0x00), 0x40, 0x4B, 0x01, 0xE2 },
		{ WRITE_COMMAND_RFM(0x01), 0xC0, 0x4B, 0x00, 0x00 },
		{ WRITE_COMMAND_RFM(0x02), 0xD0, 0xFC, 0x8C, 0x02 },
		{ WRITE_COMMAND_RFM(0x03), 0x99, 0x00, 0x39, 0x41 },
		{ WRITE_COMMAND_RFM(0x04), 0xF9, 0x96, 0x82, 0xDB },
		{ WRITE_COMMAND_RFM(0x05), 0x24, 0x06, 0x0F, 0xB6 },
		{ WRITE_COMMAND_RFM(0x06), 0x00, 0x00, 0x00, 0x00 },
		{ WRITE_COMMAND_RFM(0x07), 0x00, 0x00, 0x00, 0x00 },
		{ WRITE_COMMAND_RFM(0x08), 0x00, 0x00, 0x00, 0x00 },
		{ WRITE_COMMAND_RFM(0x09), 0x00, 0x00, 0x00, 0x00 },
		{ WRITE_COMMAND_RFM(0x0a), 0x00, 0x00, 0x00, 0x00 },
		{ WRITE_COMMAND_RFM(0x0b), 0x00, 0x00, 0x00, 0x00 },
		{ WRITE_COMMAND_RFM(0x0C), 0x00, 0x12, 0x73, 0x05 },
		{ WRITE_COMMAND_RFM(0x0D), 0x36, 0xb4, 0x80, 0x00 }
};

//Bank1 register 14
const uint8_t RFM7x_bank1R0EInit[] = {
		WRITE_COMMAND_RFM(0x0E), 0x41, 0x20, 0x08, 0x04, 0x81, 0x20, 0xCF, 0xF7, 0xFE, 0xFF, 0xFF
};

bool RFM75_Init(void){
	RFM75_hardware_init();
	RFM75_CE_PIN_low();
	return RFM75_initRegisters();
}

bool RFM75_initRegisters()
{
	// init bank 0 registers
	RFM75_selectBank(0);

	// !! The last two regs in the bank0Init list will be handled later
	for (int i = 0; i < 20; i++){
		RFM75_SPI_write_reg_val(RFM75_Bank0[i][0],
				RFM75_Bank0[i][1]);
	}

	// init address registers in bank 0
	RFM75_SPI_write_buffer((uint8_t *)RFM75_cmd_adrRX0, sizeof(RFM75_cmd_adrRX0));
	RFM75_SPI_write_buffer((uint8_t *)RFM75_cmd_adrRX1, sizeof(RFM75_cmd_adrRX1));
	RFM75_SPI_write_buffer((uint8_t *)RFM75_cmd_adrTX, sizeof(RFM75_cmd_adrTX));

	// activate Feature register
	if(!RFM75_SPI_read_reg_value(FEATURE_REG)){
		RFM75_SPI_write_buffer((uint8_t *)RFM75_cmd_activate, sizeof(RFM75_cmd_activate));
	}

	// now set Registers 1D and 1C
	RFM75_SPI_write_reg_val(RFM75_Bank0[22][0],
			RFM75_Bank0[22][1]);
	RFM75_SPI_write_reg_val(RFM75_Bank0[21][0],
			RFM75_Bank0[21][1]);

	// init bank 1 registers
	RFM75_selectBank(1);

	for (int i=0; i < 14; i++){
		RFM75_SPI_write_buffer((uint8_t *)RFM75_Bank1[i], sizeof(RFM75_Bank1[i]));
	}

	// set ramp curve
	RFM75_SPI_write_buffer((uint8_t *)RFM7x_bank1R0EInit, sizeof(RFM7x_bank1R0EInit));

	// do we have to toggle some bits here like in the example code?
	RFM75_SPI_write_buffer((uint8_t *)RFM75_cmd_tog1, sizeof(RFM75_cmd_tog1));
	RFM75_SPI_write_buffer((uint8_t *)RFM75_cmd_tog2, sizeof(RFM75_cmd_tog2));

	delay_ms(50);

	RFM75_selectBank(0);
	RFM75_set_RX_mode();

	delay_ms(5);
	RFM75_selectBank(1);
	delay_ms(5);

	//Check the ChipID
	uint8_t id = RFM75_SPI_read_reg_value(0x08);
	bool retval = true;
	if(id != 0x63)
	{
		retval=false;
	}
	RFM75_selectBank(0);
	return retval;
}

void RFM75_selectBank(uint8_t bank) 
{
	uint8_t curbank = RFM75_SPI_read_reg_value(0x07) & 0x80;
	if(curbank != bank){
		RFM75_SPI_write_buffer((uint8_t *)RFM75_cmd_switch_bank,
							sizeof(RFM75_cmd_switch_bank));
	}
}

void RFM75_set_RX_mode(void)
{
	uint8_t val;
	RFM75_SPI_write_buffer((uint8_t *)RFM75_cmd_flush_rx, sizeof(RFM75_cmd_flush_rx)); 
	val = RFM75_SPI_read_reg_value(STATUS_REG);
	RFM75_SPI_write_reg_val(WRITE_COMMAND_RFM(STATUS_REG), val);
	RFM75_CE_PIN_low();
	val = RFM75_SPI_read_reg_value(CONFIG_REG);
	val |= RFM7x_PIN_PRIM_RX;
	RFM75_SPI_write_reg_val(WRITE_COMMAND_RFM(CONFIG_REG), val);
	RFM75_CE_PIN_high();
}

void RFM75_set_RX_mode_if_needed(void){
	if (RFM75_getMode() != MODE_PRIMARY_RX)
	{
		RFM75_set_RX_mode();
	}
}

void RFM75_set_TX_mode(void) 
{
	uint8_t val;
	RFM75_SPI_write_buffer((uint8_t *)RFM75_cmd_flush_tx, sizeof(RFM75_cmd_flush_tx));
	val = RFM75_SPI_read_reg_value(STATUS_REG);
	RFM75_SPI_write_reg_val(WRITE_COMMAND_RFM(STATUS_REG), val);
	RFM75_CE_PIN_low();
	val = RFM75_SPI_read_reg_value(CONFIG_REG);
	val &= ~RFM7x_PIN_PRIM_RX;
	RFM75_SPI_write_reg_val(WRITE_COMMAND_RFM(CONFIG_REG), val);
}

void RFM75_set_TX_mode_if_needed(void){
	if (RFM75_getMode() != MODE_PRIMARY_TX)
	{
		RFM75_set_TX_mode();
	}
}

uint8_t RFM75_getMode(void) 
{
	return RFM75_SPI_read_reg_value(CONFIG_REG) & RFM7x_PIN_PRIM_RX;
}

void RFM75_setChannel(uint8_t cnum) 
{
	RFM75_SPI_write_reg_val( RFM7x_CMD_WRITE_REG | RF_CH_REG, cnum);
}

uint8_t RFM75_getChannel(void) 
{
	return RFM75_SPI_read_reg_value(RF_CH_REG);
}

void RFM75_setPower(uint8_t pwr) 
{
	if (pwr > 3) {
		return;
	}
	rfSetupReg_t rf_settings;
	rf_settings.all= RFM75_SPI_read_reg_value(RF_SETTINGS_REG);
	rf_settings.rf_power = pwr;
	RFM75_SPI_write_reg_val(WRITE_COMMAND_RFM(RF_SETTINGS_REG), rf_settings.all);
}

uint8_t RFM75_configRxPipe(uint8_t pipe_nr,
		uint8_t *adr, // Must be 5 bytes long !
		uint8_t pipe_length,
		uint8_t en_aa)
{

	uint8_t tmp;

	if(pipe_length > 32 || pipe_nr > 5 || en_aa > 1){
		return 0;
	}

	if(pipe_nr < 2){
		RFM75_SPI_write_buffer_at_start_register(WRITE_COMMAND_RFM((RX_PIPE_0_ADDR_REG + pipe_nr)), adr, 5);
	}else{
		RFM75_SPI_write_reg_val(WRITE_COMMAND_RFM((RX_PIPE_0_ADDR_REG + pipe_nr)), adr[0]);
	}

	// static
	if (pipe_length) {
		// set payload len
		RFM75_SPI_write_reg_val(WRITE_COMMAND_RFM((RX_PIPE_0_PAYLOAD_LENGTH_REG + pipe_nr)), pipe_length);
		// set EN_AA bit
		tmp = RFM75_SPI_read_reg_value(ENABLE_AUTO_ACK_REG);
		if (en_aa){
			tmp |= 1 << pipe_nr;
		}else{
			tmp &= ~(1 << pipe_nr);
		}
		RFM75_SPI_write_reg_val(WRITE_COMMAND_RFM(ENABLE_AUTO_ACK_REG), tmp);
		// clear DPL bit
		tmp = RFM75_SPI_read_reg_value(DYNAMIC_PAYLOAD_LENGTH_REG);
		tmp &= ~(1 << pipe_nr);
	}else{
		// set payload len to default
		RFM75_SPI_write_reg_val(WRITE_COMMAND_RFM(RX_PIPE_0_PAYLOAD_LENGTH_REG + pipe_nr), 0x20);
		// set EN_AA bit
		tmp = RFM75_SPI_read_reg_value(ENABLE_AUTO_ACK_REG);
		tmp |= 1 << pipe_nr;
		RFM75_SPI_write_reg_val(WRITE_COMMAND_RFM(ENABLE_AUTO_ACK_REG), tmp);
		// set DPL bit
		tmp = RFM75_SPI_read_reg_value(DYNAMIC_PAYLOAD_LENGTH_REG);
		tmp |= 1 << pipe_nr;
	}

	RFM75_SPI_write_reg_val(WRITE_COMMAND_RFM(DYNAMIC_PAYLOAD_LENGTH_REG), tmp);
	RFM75_enableRxPipe(pipe_nr);

	return 1;
}

void RFM75_enableRxPipe(uint8_t pipe_nr) 
{
	if (pipe_nr > 5) {
		return;
	}
	uint8_t tmp;
	tmp = RFM75_SPI_read_reg_value(ENABLE_RX_ADDR_REG);
	tmp |= 1 << pipe_nr;
	RFM75_SPI_write_reg_val(WRITE_COMMAND_RFM(ENABLE_RX_ADDR_REG), tmp);
}

void RFM75_disableRxPipe(uint8_t pipe_nr) 
{
	if (pipe_nr > 5){
		return;
	}
	uint8_t tmp;
	tmp = RFM75_SPI_read_reg_value(ENABLE_RX_ADDR_REG);
	tmp &= ~(1 << pipe_nr);
	RFM75_SPI_write_reg_val(WRITE_COMMAND_RFM(ENABLE_RX_ADDR_REG), tmp);
}

void RFM75_configTxPipe(uint8_t * adr, uint8_t pltype) 
{
	RFM75_SPI_write_buffer_at_start_register(WRITE_COMMAND_RFM(TX_ADDR_REG), adr, 5);
	RFM75_SPI_write_buffer_at_start_register(WRITE_COMMAND_RFM(RX_PIPE_0_ADDR_REG), adr, 5);
	// set static or dynamic payload on the receive pipe
	uint8_t tmp;
	tmp = RFM75_SPI_read_reg_value(DYNAMIC_PAYLOAD_LENGTH_REG);
	if(pltype == TX_DPL){
		tmp |= 1;
	}else{
		tmp &= ~(1 << 0);
	}
	RFM75_SPI_write_reg_val(WRITE_COMMAND_RFM(DYNAMIC_PAYLOAD_LENGTH_REG), tmp);
}

uint8_t RFM75_sendPayload(const uint8_t * payload,
		const uint8_t len,
		const uint8_t toAck)
{
	RFM75_CE_PIN_high();

	StatusReg_t status;
	status.all = RFM75_SPI_read_reg_value(FIFO_STATUS_REG);
	if (status.tx_fifo_full)
	{
		return 0;
	}

	uint8_t cmd;

	if(toAck == -1){
		cmd = RFM7x_CMD_W_ACK_PAYLOAD;
	}else if (toAck == 0){
		cmd = RFM7x_CMD_W_TX_PAYLOAD_NOACK;
	}else{
		cmd = RFM7x_CMD_WR_TX_PLOAD;
	}
	RFM75_SPI_write_buffer_at_start_register(cmd, payload, len);
	return 1;
}

/* returns the length in bytes */
uint8_t RFM75_Receive_bytes(uint8_t *payload)
{
	uint8_t len;
	StatusReg_t status;
	StatusRegFifo_t fifo_status;

	status.all = RFM75_SPI_read_reg_value(STATUS_REG);
	if((bool)status.rx_data_ready && (status.all != 0xFF)){
		len = RFM75_SPI_read_reg_value(RFM7x_CMD_RX_PL_WID); // Payload width
		RFM75_SPI_read_buffer(RFM7x_CMD_RD_RX_PLOAD, payload, len);
		fifo_status.all = RFM75_SPI_read_reg_value(FIFO_STATUS_REG);

		if (fifo_status.rx_empty) {
			status.rx_data_ready = 1; // clear status bit by setting it to 1
			RFM75_SPI_write_reg_val(WRITE_COMMAND_RFM(STATUS_REG), status.all);
		}
		return len;
	}else{
		return 0;
	}
}

uint16_t RFM75_Receive_bytes_feedback(uint8_t *payload){
	uint8_t len;
	StatusReg_t status;
	StatusRegFifo_t fifo_status;

	status.all = RFM75_SPI_read_reg_value(STATUS_REG);
	if((bool)status.rx_data_ready && (status.all != 0xFF)){
		len = RFM75_SPI_read_reg_value(RFM7x_CMD_RX_PL_WID); // Payload width
		RFM75_SPI_read_buffer(RFM7x_CMD_RD_RX_PLOAD, payload, len);
		fifo_status.all = RFM75_SPI_read_reg_value(FIFO_STATUS_REG);

		if (fifo_status.rx_empty) {
			// status.all |= 0x40; // clear status bit rx_dr
			status.rx_data_ready = 1; // clear status bit by setting it to 1
			RFM75_SPI_write_reg_val(WRITE_COMMAND_RFM(STATUS_REG), status.all);
		}
		return ((status.all << 8) | fifo_status.all);
	}else{
		return 0;
	}
}

void RFM75_flushTxFIFO() 
{
	RFM75_SPI_write_buffer((uint8_t *)RFM75_cmd_flush_tx, sizeof(RFM75_cmd_flush_tx));
}

void RFM75_flushRxFIFO() 
{
	RFM75_SPI_write_buffer((uint8_t *)RFM75_cmd_flush_rx, sizeof(RFM75_cmd_flush_rx));
}

void RFM75_turnOn()
{
	uint8_t status = RFM75_SPI_read_reg_value(STATUS_REG);
	status |= PWR_BIT;
	RFM75_SPI_write_reg_val(WRITE_COMMAND_RFM(STATUS_REG), status);
}

void RFM75_turnOff()
{
	uint8_t status = RFM75_SPI_read_reg_value(STATUS_REG);
	status &= ~PWR_BIT;
	RFM75_SPI_write_reg_val(WRITE_COMMAND_RFM(STATUS_REG), status);
	RFM75_CE_PIN_low();
}

TransmitResult_t RFM75_Transmit_bytes(const uint8_t *buff,
									  const uint32_t length,
									  const uint32_t maxTimeoutUs,
									  bool requestAck)
{
	TransmitResult_t result;
	uint32_t i=0;
	bool readStatus = true;
	StatusReg_t status;
	status.all = 0;

	RFM75_sendPayload(buff, length, (uint8_t)requestAck);

	rxtx_interrupt = 0;
	while((rxtx_interrupt == 0) &&
			(i < maxTimeoutUs)){
#define TIMEOUT_uS 20
		_delay_us(TIMEOUT_uS);
		status.all = RFM75_SPI_read_reg_value(STATUS_REG);
		if(status.rx_pipe_num == 0x7){
			// Missing IRQ, stop
			readStatus = false;
			break;
		}
		i = i + TIMEOUT_uS;
	}

	rxtx_interrupt = 0;
	/* Clear ALL the status interrupts.
	 * When writing a bit that is set to 1,
	 * it clears the bits, and also the MAX_RT BIT
	 */
	RFM75_SPI_write_reg_val(WRITE_COMMAND_RFM(STATUS_REG), status.all);

	if (readStatus){
		status.all = RFM75_SPI_read_reg_value(STATUS_REG);
	}
	if (status.tx_data_sent){
		result.status = SUCCESS;
		result.bytesSent = length;
	}
	else if (status.max_retransmits){
		result.status = MAXRT;
		result.bytesSent = 0;
	}
	else if (status.tx_fifo_full){
		result.status = FIFOFULL;
		result.bytesSent = 0;
	}

	if (i >= maxTimeoutUs){
		result.status = UNKNOWN;
		result.bytesSent = 0;
	}
	return result;
}

void RFM75_prepareForListening(const uint8_t * address){
	RFM75_set_RX_mode_if_needed();
	RFM75_configRxPipe(0, address, 0, 1);
}

void RFM75_startListening(const uint8_t channel, const uint8_t * address){
	RFM75_prepareForListening(address);
	RFM75_setChannel(channel);
	RFM75_CE_PIN_high();
}

void RFM75_PinInterruptHandler(void){
	rxtx_interrupt = 1;
	asm("NOP");
}
