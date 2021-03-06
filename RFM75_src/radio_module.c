#include "base.h"
#include "RFM75_driver.h"
#include "radio_module.h"
#include "delay_us.h"
#include "RFM75_codes.h"

const AddressAndChannel_t default_RFM75_Addr = {
			{0x35, 0xAF, 0x42, 0x23, 0x99}, /* ADDRESS */
			0 /* RF Channel */
};

_STATIC_ _INLINE_ void RFM75_flush_tx_FIFO();
_STATIC_ _INLINE_ void RFM75_flush_rx_FIFO();

/*
 * The order of modifying the registers should be the same
 * as in the datasheet.
 * F.E If you want to modify the setting of a receive pipe,
 * f.e acknoledge bits and dynamic payload length
 * do it in ascending order of the registers in the datasheet.
 */
extern volatile uint32_t rxtx_interrupt;

_STATIC_ const uint8_t RFM75_cmd_adrTX[]  = { WRITE_COMMAND_RFM(0x10), 0x35, 0xAF, 0x42, 0x23, 0x99};
_STATIC_ const uint8_t RFM75_cmd_adrRX0[] = { WRITE_COMMAND_RFM(0x0A), 0x35, 0xAF, 0x42, 0x23, 0x99};
_STATIC_ const uint8_t RFM75_cmd_adrRX1[] = { WRITE_COMMAND_RFM(0x0B), 0x45, 0x43, 0x10, 0x10, 0x02};
_STATIC_ const uint8_t RFM75_cmd_switch_bank[] = { 0x50, 0x53 };
_STATIC_ const uint8_t RFM75_cmd_flush_rx_fifos[] = { 0xe2, 0x00 };
_STATIC_ const uint8_t RFM75_cmd_flush_tx_fifos[] = { 0xe1, 0x00 };
_STATIC_ const uint8_t RFM75_cmd_activate[] = { ACTIVATE, ACTIVATE_BYTE }; // Activates
_STATIC_ const uint8_t RFM75_cmd_tog1[] = { WRITE_COMMAND_RFM(0x04), 0xF9 | 0x06, 0x96, 0x82, 0xDB };
_STATIC_ const uint8_t RFM75_cmd_tog2[] = { WRITE_COMMAND_RFM(0x04), 0xF9, 0x96, 0x82, 0xDB};

_STATIC_ const uint8_t  RFM75_Bank0[][2] = {
		// address data
		{ WRITE_COMMAND_RFM(CONFIG_REG), (EN_CRC | CRCO | PWR_UP | PRIM_RX)},
		{ WRITE_COMMAND_RFM(ENABLE_AUTO_ACK_REG), ALL_6_PIPES },
		{ WRITE_COMMAND_RFM(ENABLE_RX_ADDR_REG), ALL_6_PIPES },
		{ WRITE_COMMAND_RFM(SETUP_ADDR_WIDTH), ADDRESS_WIDTH_5 },
		// AUTO_RETRANSMISSION_DELAY is given in multiples of 250 uS
		{ WRITE_COMMAND_RFM(SETUP_RETR), ( AUTO_RETR_DELAY(15) | \
						   AUTO_RETR_COUNT(15)) }, // 4 ms retr. delay
		{ WRITE_COMMAND_RFM(RF_CH_REG), RF_CHANNEL(0x17) },
		{ WRITE_COMMAND_RFM(RF_SETTINGS_REG), 0x2F },
		{ WRITE_COMMAND_RFM(STATUS_REG), 0x07 },
		{ WRITE_COMMAND_RFM(OBSERVE_TX_REG), 0x00 },
		{ WRITE_COMMAND_RFM(CARRIER_DETECT_REG), 0x00 },
		{ WRITE_COMMAND_RFM(RX_PIPE_2_ADDR_REG), 0xC3 },
		{ WRITE_COMMAND_RFM(RX_PIPE_3_ADDR_REG), 0xC4 },
		{ WRITE_COMMAND_RFM(RX_PIPE_4_ADDR_REG), 0xC5 },
		{ WRITE_COMMAND_RFM(RX_PIPE_5_ADDR_REG), 0xC6 },
		{ WRITE_COMMAND_RFM(RX_PIPE_0_PAYLOAD_LENGTH_REG), 32 },
		{ WRITE_COMMAND_RFM(RX_PIPE_1_PAYLOAD_LENGTH_REG), 32 },
		{ WRITE_COMMAND_RFM(RX_PIPE_2_PAYLOAD_LENGTH_REG), 32 },
		{ WRITE_COMMAND_RFM(RX_PIPE_3_PAYLOAD_LENGTH_REG), 32 },
		{ WRITE_COMMAND_RFM(RX_PIPE_4_PAYLOAD_LENGTH_REG), 32 },
		{ WRITE_COMMAND_RFM(RX_PIPE_5_PAYLOAD_LENGTH_REG), 32 },
		{ WRITE_COMMAND_RFM(FIFO_STATUS_REG), 0x00 },
		{ WRITE_COMMAND_RFM(DYNAMIC_PAYLOAD_LENGTH_REG), ALL_6_PIPES},
/*		{ WRITE_COMMAND_RFM(FEATURE_REG), (EN_DYNAMIC_PAYLOAD_LENGTH | \
		EN_PAYLOAD_WITH_ACK) } */
		{ WRITE_COMMAND_RFM(FEATURE_REG), (EN_DYNAMIC_PAYLOAD_LENGTH | \
						   EN_PAYLOAD_WITH_ACK | \
						   EN_SPECIAL_NOACK_COMMAND) } 
};

//************ Bank1 register initialization commands
_STATIC_ const uint8_t RFM75_Bank1[][5] = {
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

_STATIC_ const uint8_t RFM7x_bank1_ramp_curve[] = {
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

	RFM75_SPI_write_buffer((uint8_t *)RFM7x_bank1_ramp_curve, sizeof(RFM7x_bank1_ramp_curve));

	// do we have to toggle some bits here like in the example code?
	RFM75_SPI_write_buffer((uint8_t *)RFM75_cmd_tog1, sizeof(RFM75_cmd_tog1));
	RFM75_SPI_write_buffer((uint8_t *)RFM75_cmd_tog2, sizeof(RFM75_cmd_tog2));

	_delay_ms(50);

	RFM75_selectBank(0);
	RFM75_set_RX_mode();

	_delay_ms(5);
	RFM75_selectBank(1);
	_delay_ms(5);

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
	RFM75_CE_PIN_low();
	RFM75_flush_rx_FIFO();

	val = RFM75_SPI_read_reg_value(STATUS_REG);
	val |= (TX_DATA_SENT_FLAG | RX_DATA_READY_FLAG | MAX_RETRANSMITS_FLAG);
	RFM75_SPI_write_reg_val(WRITE_COMMAND_RFM(STATUS_REG), val);

	val = RFM75_SPI_read_reg_value(CONFIG_REG);
	val |= PRIM_RX;
	RFM75_SPI_write_reg_val(WRITE_COMMAND_RFM(CONFIG_REG), val);
}

void RFM75_set_RX_mode_if_needed(void){
	if (RFM75_getMode() != PRIM_RX)
	{
		RFM75_set_RX_mode();
	}
}

void RFM75_reset_interrupts(void){
	uint8_t val;
	val = RFM75_SPI_read_reg_value(STATUS_REG);
	val |= (TX_DATA_SENT_FLAG | RX_DATA_READY_FLAG | MAX_RETRANSMITS_FLAG);
	RFM75_SPI_write_reg_val(WRITE_COMMAND_RFM(STATUS_REG), val);
}

void RFM75_set_TX_mode(void) 
{
	uint8_t val;
	RFM75_CE_PIN_low();

	RFM75_reset_interrupts();

	val = RFM75_SPI_read_reg_value(CONFIG_REG);
	val &= ~PRIM_RX;
	RFM75_SPI_write_reg_val(WRITE_COMMAND_RFM(CONFIG_REG), val);

	RFM75_flush_tx_FIFO();
}

void RFM75_set_TX_mode_if_needed(void){
	if (RFM75_getMode() != MODE_PRIMARY_TX)
	{
		RFM75_set_TX_mode();
	}
}

uint8_t RFM75_getMode(void) 
{
	return RFM75_SPI_read_reg_value(CONFIG_REG) & PRIM_RX;
}

void RFM75_setChannel(uint8_t cnum) 
{
	RFM75_SPI_write_reg_val( WRITE_COMMAND_RFM(RF_CH_REG), cnum);
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

void RFM75_configRxPipe(uint8_t pipe_nr,
		const uint8_t *adr, // Must be 5 bytes long !
		uint8_t pipe_length,
		bool enable_auto_ack)
{
	uint8_t tmp;

	if(pipe_length > 32 || pipe_nr > 5){
		return;
	}

	if(pipe_nr < 2){
		RFM75_SPI_write_buffer_at_start_register(WRITE_COMMAND_RFM((RX_PIPE_0_ADDR_REG + pipe_nr)), adr, 5);
	}else{
		RFM75_SPI_write_reg_val(WRITE_COMMAND_RFM((RX_PIPE_0_ADDR_REG + pipe_nr)), adr[0]);
	}

	tmp = RFM75_SPI_read_reg_value(ENABLE_AUTO_ACK_REG);
	if (enable_auto_ack){
		tmp |= 1 << pipe_nr;
	}else{
		tmp &= ~(1 << pipe_nr);
	}
	RFM75_SPI_write_reg_val(WRITE_COMMAND_RFM(ENABLE_AUTO_ACK_REG), tmp);

	if (pipe_length > 0) {
		tmp = RFM75_SPI_read_reg_value(DYNAMIC_PAYLOAD_LENGTH_REG);
		tmp &= ~(1 << pipe_nr);
		RFM75_SPI_write_reg_val(WRITE_COMMAND_RFM(DYNAMIC_PAYLOAD_LENGTH_REG), tmp);		
	}else{
		tmp = RFM75_SPI_read_reg_value(DYNAMIC_PAYLOAD_LENGTH_REG);
		tmp |= 1 << pipe_nr;
		RFM75_SPI_write_reg_val(WRITE_COMMAND_RFM(DYNAMIC_PAYLOAD_LENGTH_REG), tmp);		
	}
	RFM75_enableRxPipe(pipe_nr);
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

void RFM75_configTxPipe(const uint8_t * adr, bool enable_dynamic_payload)
{
	RFM75_SPI_write_buffer_at_start_register(WRITE_COMMAND_RFM(TX_ADDR_REG), adr, 5);
	RFM75_SPI_write_buffer_at_start_register(WRITE_COMMAND_RFM(RX_PIPE_0_ADDR_REG), adr, 5);
	// set static or dynamic payload on the receive pipe
	uint8_t dyn_pl_reg;
	dyn_pl_reg = RFM75_SPI_read_reg_value(DYNAMIC_PAYLOAD_LENGTH_REG);
	if(enable_dynamic_payload){
		dyn_pl_reg |= (1 << PIPE_0);
	}else{
		dyn_pl_reg &= ~(1 << PIPE_0);
	}
	RFM75_SPI_write_reg_val(WRITE_COMMAND_RFM(DYNAMIC_PAYLOAD_LENGTH_REG), dyn_pl_reg);
}

/**
 * @brief function to read out the bytes from RFM75 module
 * @return length in bytes
 */
uint8_t RFM75_Receive_bytes(uint8_t *payload)
{
	uint8_t len;
	StatusReg_t status;
	FifoStatusReg_t fifo_status;

	status.all = RFM75_SPI_read_reg_value(STATUS_REG);
	if((bool)status.rx_data_ready && (status.all != 0xFF)){
		len = RFM75_SPI_read_reg_value(READ_PAYLOAD_LENGTH); // Payload width
		RFM75_SPI_read_buffer(READ_RX_PAYLOAD, payload, len);
		fifo_status.all = RFM75_SPI_read_reg_value(FIFO_STATUS_REG);

		if (fifo_status.rx_empty){
			status.rx_data_ready = 1; // clear status bit by setting it to 1
			RFM75_SPI_write_reg_val(WRITE_COMMAND_RFM(STATUS_REG), status.all);
		}
		return len;
	}else{
		return 0;
	}
}

_STATIC_ _INLINE_ CombinedReg_t combinedReg(uint8_t msglen, uint8_t fifo, uint8_t observe_tx, uint8_t status){
	CombinedReg_t creg;
	creg.all = ((((uint32_t) msglen) << 24) |
				(((uint32_t)fifo << 16)) |
				(((uint32_t)observe_tx << 8)) |
				status);
	return creg;
}

CombinedReg_t RFM75_Receive_bytes_feedback(uint8_t *payload){
	StatusReg_t status;

	status.all = RFM75_SPI_read_reg_value(STATUS_REG);
	if((bool)status.rx_data_ready && (status.all != 0xFF)){
		return RFM75_Receive_bytes_when_data_present(status, payload);
	}else{
		return combinedReg(0, 0, 0, status.all);
	}
}

CombinedReg_t RFM75_Receive_bytes_when_data_present(StatusReg_t incoming_status, uint8_t *receive_buffer){
	FifoStatusReg_t fifo_status;
	StatusReg_t fresh_status;
	uint8_t cnt=0;
	uint8_t len=0;

	if(receive_buffer == NULL){
		return combinedReg(0,0,0,0);
	}
	do{
		len = RFM75_SPI_read_reg_value(READ_PAYLOAD_LENGTH);
		RFM75_SPI_read_buffer(READ_RX_PAYLOAD, receive_buffer, len);
		fresh_status.all = RFM75_SPI_read_reg_value(STATUS_REG);
		fifo_status.all = RFM75_SPI_read_reg_value(FIFO_STATUS_REG);

		if (fifo_status.rx_empty){
			fresh_status.rx_data_ready = 1; // clear status bit by setting it to 1
			RFM75_SPI_write_reg_val(WRITE_COMMAND_RFM(STATUS_REG), fresh_status.all);
		}
		cnt +=1;
	}while(fifo_status.rx_empty != 1);
	return combinedReg(len, fifo_status.all, cnt, (incoming_status.all | fresh_status.all));
}

_STATIC_ _INLINE_ void RFM75_flush_tx_FIFO()
{
	RFM75_SPI_write_buffer((uint8_t *)RFM75_cmd_flush_tx_fifos, sizeof(RFM75_cmd_flush_tx_fifos));
}

_STATIC_ _INLINE_ void RFM75_flush_rx_FIFO()
{
	RFM75_SPI_write_buffer((uint8_t *)RFM75_cmd_flush_rx_fifos, sizeof(RFM75_cmd_flush_rx_fifos));
}

/*
 * Setting the CE Pin High is still necessary to f.e make the Chip transmit.
 */
void RFM75_turn_on(uint8_t config_mask)
{
	ConfigReg_t config;
	config.all = RFM75_SPI_read_reg_value(CONFIG_REG);
	config.power_up = 1;
	config.all |= config_mask;
	RFM75_SPI_write_reg_val(WRITE_COMMAND_RFM(CONFIG_REG), config.all);
}

void RFM75_turn_off()
{
	ConfigReg_t config;
	config.all = RFM75_SPI_read_reg_value(CONFIG_REG);
	config.power_up = 0;
	RFM75_SPI_write_reg_val(WRITE_COMMAND_RFM(CONFIG_REG), config.all);
	RFM75_CE_PIN_low();
}

CombinedReg_t RFM75_Transmit_bytes(const uint8_t *send_payload,
								 	 const uint8_t length,
									 const uint32_t maxTimeoutUs,
									 uint8_t *receive_payload,
									 bool expected_ack_data){
	uint32_t timeout=0;
	StatusReg_t status;
	status.all = 0;

	status.all = RFM75_SPI_read_reg_value(STATUS_REG);
	if(status.tx_fifo_full){
		RFM75_flush_tx_FIFO();
	}
	RFM75_SPI_write_reg_val(WRITE_COMMAND_RFM(STATUS_REG), (status.all | \
															TX_DATA_SENT_FLAG | \
															MAX_RETRANSMITS_FLAG));
	RFM75_SPI_write_buffer_at_start_register(WRITE_TX_PAYLOAD, send_payload, length);
	rxtx_interrupt = 0;
	RFM75_CE_PIN_high();

	while((rxtx_interrupt == 0) && (timeout < maxTimeoutUs)){
#define DELAY_TIME 50
		_delay_us(DELAY_TIME);
		timeout += DELAY_TIME;
	}
	/* Clear ALL the status interrupts.
	 * When writing a bit that is set to 1,
	 * it clears the bits, and also the MAX_RT BIT
	 */
	RFM75_CE_PIN_low();

	status.all = RFM75_SPI_read_reg_value(STATUS_REG);
	RFM75_SPI_write_reg_val(WRITE_COMMAND_RFM(STATUS_REG),
			(status.all | \
					TX_DATA_SENT_FLAG | \
					MAX_RETRANSMITS_FLAG));
	rxtx_interrupt = 0;
	if(expected_ack_data && (status.rx_data_ready == 1)){
			return RFM75_Receive_bytes_when_data_present(status, receive_payload);
	}
	return combinedReg(0, 0, 0, status.all);
}

void RFM75_prepareForListening(const AddressAndChannel_t * address_and_channel){
	if(address_and_channel == NULL){
		return;
	}
	RFM75_set_RX_mode();
	RFM75_configRxPipe(0 		/* Pipe number */ ,
					   address_and_channel->address,
					   0, 		/* Static = 1, Dynamic = 0 */
					   true); 	/*	Enable Auto Acknowledge */
	RFM75_setChannel(address_and_channel->channel);
}

void RFM75_prepareForTransmission(const AddressAndChannel_t * address_and_channel){
	RFM75_configTxPipe(address_and_channel->address, true);
	RFM75_set_TX_mode();
	RFM75_setChannel(address_and_channel->channel);
	RFM75_turn_on((HIDE_DATA_RECEIVED_INTERRUPT));
}

void RFM75_startListening(const AddressAndChannel_t * address_and_channel){
	if(address_and_channel == NULL){
		return;
	}
	RFM75_prepareForListening(address_and_channel);
	RFM75_turn_on((HIDE_TX_DATA_SENT_INTERRUPT | HIDE_MAX_RETRANSMITS_INTERRUPT));
	RFM75_CE_PIN_high();
}
