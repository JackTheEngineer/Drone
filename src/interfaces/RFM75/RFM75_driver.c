/*
 * RFM75_driver.c
 *
 *  Created on: Oct 25, 2017
 *      Author: chocolate
 */

#include "RFM75_driver.h"
#include "delay.h"

#undef PRIMARY_RX_MODE


_STATIC_ const uint8_t remote_control_address[ADDRESS_SIZE] = {0x00, 0x00, 0x4C, 0xC0, 0x91};
_STATIC_ const uint8_t drone_address[ADDRESS_SIZE] = {0x00, 0x00, 0xD4, 0x09, 0xEE};

#ifdef PRIMARY_RX_MODE
_STATIC_ const uint8_t config_remote_control_rfm[][2] = {
		{WRITE_COMMAND_RFM(CONFIG), (MASK_MAX_RT | PWR_UP)},
		{WRITE_COMMAND_RFM(EN_AA), (ENAA_P0)},
		{WRITE_COMMAND_RFM(EN_RXADDR), (ERX_P0)},
		{WRITE_COMMAND_RFM(SETUP_AW), ADDRESS_WIDTH_5},
		{WRITE_COMMAND_RFM(SETUP_RETR), ((ARC_CNT_mask & 0b10)| (PLOS_CNT_mask & (0b00000000)))},
		{WRITE_COMMAND_RFM(RF_CH), 30}, /* Frequency channel, says to have 6 bits */
		{WRITE_COMMAND_RFM(RF_SETUP), (RF_PWR(3)| LNA_HCURR)},
		{WRITE_COMMAND_RFM(FEATURE), 0},
};
#endif /* PRIMARTY_RX_MODE */

/*
 * First byte is the command byte, or the byte
 */
_STATIC_ const uint8_t reg_bank_1[][5] = {
	{WRITE_COMMAND_RFM(00), 0x40, 0x4B, 0x01, 0xe2},
	{WRITE_COMMAND_RFM(01), 0xC0, 0x4B, 0x00, 0x00},
	{WRITE_COMMAND_RFM(02), 0xD0, 0xFC, 0x8C, 0x02},
	{WRITE_COMMAND_RFM(03), 0x99, 0x00, 0x39, 0x21},
	{WRITE_COMMAND_RFM(04), 0xF9, 0x96, 0x82, 0x1B}, /* 1 Msps */
	{WRITE_COMMAND_RFM(05), 0x24, 0x02, 0x0F, 0xA6}, /* 1 Msps, disable rssi */
	/* Least significant byte first again */
	{WRITE_COMMAND_RFM(0x0C), 0x00, 0x12, 0x73, 0x05}, /* PLL Mode 120 us, or something*/
	{WRITE_COMMAND_RFM(0x0D), 0x36, 0xB4, 0x80, 0x00}, /* NEW_FEATURE */
};
uint8_t zero_bytes[12] = {0,0,0,0,
							0,0,0,0,
							0,0,0,0};

_STATIC_ const uint8_t ramp_curve[12] = {
	WRITE_COMMAND_RFM(0x0E),
	0x41, 0x20, 0x08,
	0x04, 0x81,
	0x20, 0xcf, 0xf7,
	0xfe, 0xff, 0xff,
};

#ifndef PRIMARY_RX_MODE
_STATIC_ const uint8_t config_drone_rfm[][2] ={
		{WRITE_COMMAND_RFM(CONFIG), (MASK_MAX_RT | PRIM_RX | PWR_UP)},
		{WRITE_COMMAND_RFM(EN_AA), (ENAA_P0)},
		{WRITE_COMMAND_RFM(EN_RXADDR), (ERX_P0)},
		{WRITE_COMMAND_RFM(SETUP_AW), ADDRESS_WIDTH_5},
		{WRITE_COMMAND_RFM(SETUP_RETR), ((ARC_CNT_mask & 0b10)| (PLOS_CNT_mask & (0b0000)))},
		{WRITE_COMMAND_RFM(RF_CH), 30}, /* Frequency channel, says to have 7 bits */
		{WRITE_COMMAND_RFM(RF_SETUP), (RF_PWR(3) | LNA_HCURR)},
		{WRITE_COMMAND_RFM(FEATURE), 0},
};
#endif

#ifdef PRIMARY_RX_MODE
_STATIC_ uint8_t const * const tx_address = drone_address;
_STATIC_ uint8_t const * const rx_address = remote_control_address;
_STATIC_ uint8_t const *config_rfm = &config_remote_control_rfm[0][0];
#else
_STATIC_ uint8_t const * const tx_address = remote_control_address;
_STATIC_ uint8_t const * const rx_address = drone_address;
_STATIC_ uint8_t const *config_rfm = &config_drone_rfm[0][0];
#endif
_STATIC_ uint8_t const config_size = 8;
_STATIC_ uint8_t const bank1_config_size = 8;
_STATIC_ uint8_t const * const bank1_config_rfm = &reg_bank_1[0][0];

static volatile uint8_t rxtx_interrupt;

void RFM75_set_address(uint8_t command, uint8_t const *address);
uint8_t RFM75_current_bank(void);
void RFM75_Init_Bank0(void);
void RFM75_Init_Bank1(void);
void RFM75_SwitchBank(void);
void RFM75_Init_RAMP(uint8_t const *initval);
void RFM75_set_Bank0_zero(void);
void RFM75_set_Bank1_zero(void);
void RFM75_set_bank(uint8_t bank);
void RFM75_toggle_reg4_bits(void);
void RFM75_flush_both_rx_tx_fifos(void);
void RFM75_configure_as_receiver(void);
void RFM75_configure_as_transmitter(void);

void RFM75_Init(void){
	RC_Iface_init();

	RFM75_set_bank(0);
	RFM75_set_Bank0_zero();
	RFM75_set_bank(1);
	RFM75_set_Bank1_zero();
	delay_ms(200);
	RFM75_set_bank(0);
	RFM75_Init_Bank0();
	RFM75_set_bank(1);
	RFM75_Init_Bank1();
	RFM75_set_bank(0);
	RFM75_flush_both_rx_tx_fifos();
	delay_ms(50);
#ifdef PRIMARY_RX_MODE
	RFM75_configure_as_receiver();
#else
	RFM75_configure_as_transmitter();
#endif
}

void RFM75_configure_as_receiver(void){
	uint8_t sendbytes[2] = {RX_PW_P0, DATASIZE_RFM75_TRANSMIT};
	/* This signal tells the chip */
        /* to get ready to receive any incoming things */
	RC_Iface_CE_high();
        /* Set the expeced datawidth for receive */
	RC_Iface_send_bytes(sendbytes, 2, DISABLE_CE);
}

void RFM75_configure_as_transmitter(void){
	RC_Iface_CE_low();
}

uint8_t RFM75_current_bank(void){
	uint8_t readmsg[2] = {STATUS, 0};
	RC_Iface_read_bytes(readmsg, 2, DISABLE_CE);
	/* The seventh bit is either bank 1 or bank 0 */
	return ((readmsg[1] & (1<<7)) >> 7);
}

void RFM75_set_Bank0_zero(void){
	uint8_t i;
	for(i=0; i <= 0x09; i++){
		zero_bytes[0] = WRITE_COMMAND_RFM(i);
		RC_Iface_send_bytes(zero_bytes, 2, DISABLE_CE);
	}
	zero_bytes[0] = WRITE_COMMAND_RFM(RX_ADDR_P0);
	RC_Iface_send_bytes(zero_bytes, ADDRESS_SIZE+1, DISABLE_CE);
	zero_bytes[0] = WRITE_COMMAND_RFM(RX_ADDR_P1);
	RC_Iface_send_bytes(zero_bytes, ADDRESS_SIZE+1, DISABLE_CE);
	zero_bytes[0] = WRITE_COMMAND_RFM(TX_ADDR);
	RC_Iface_send_bytes(zero_bytes, ADDRESS_SIZE+1, DISABLE_CE);
	for(i=RX_ADDR_P2; i <= 0x0F; i++){
		zero_bytes[0] = WRITE_COMMAND_RFM(i);
		RC_Iface_send_bytes(zero_bytes, 2, DISABLE_CE);
	}
	for(i=RX_PW_P0; i <= FIFO_STATUS; i++){
		zero_bytes[0] = WRITE_COMMAND_RFM(i);
		RC_Iface_send_bytes(zero_bytes, 2, DISABLE_CE);
	}
	for(i=DYNPD; i <= FEATURE; i++){
		zero_bytes[0] = WRITE_COMMAND_RFM(i);
		RC_Iface_send_bytes(zero_bytes, 2, DISABLE_CE);
	}
}

void RFM75_set_Bank1_zero(void){
	uint8_t i;
	for(i=0; i <= 0x09; i++){
		zero_bytes[0] = WRITE_COMMAND_RFM(i);
		RC_Iface_send_bytes(zero_bytes, 5, DISABLE_CE);
	}
	zero_bytes[0] = WRITE_COMMAND_RFM(0x0E);
	RFM75_Init_RAMP(zero_bytes);
}

void RFM75_Init_Bank0(void){
	uint8_t i;
	for(i=0; i < config_size; i++){
		RC_Iface_send_bytes(&config_rfm[i*2], 2, DISABLE_CE);
	}
	RFM75_set_address(WRITE_COMMAND_RFM(TX_ADDR),
			  tx_address);
	RFM75_set_address(WRITE_COMMAND_RFM(RX_ADDR_P0),
			  rx_address);
}

void RFM75_Init_Bank1(void){
	uint8_t i;
	for(i=0; i < bank1_config_size; i++){
		RC_Iface_send_bytes(&bank1_config_rfm[i*5], 5, DISABLE_CE);
	}
	RFM75_Init_RAMP(ramp_curve);
	RFM75_toggle_reg4_bits();
}

void RFM75_SwitchBank(void){
	uint8_t sendbytes[2] = {ACTIVATE, 0x53};
	RC_Iface_send_bytes(sendbytes, 2, DISABLE_CE);
}

void RFM75_Init_RAMP(uint8_t const *initval){
	RC_Iface_send_bytes(initval, 12, DISABLE_CE);
}

void RFM75_set_address(uint8_t command, uint8_t const *address){
	uint8_t address_send_bytes[ADDRESS_SIZE + 1];
	uint8_t addr_count;
	address_send_bytes[0] = command;
	for(addr_count = 1;
			addr_count < (ADDRESS_SIZE+1);
			addr_count++){
		address_send_bytes[addr_count] = address[addr_count-1];
	}
	RC_Iface_send_bytes(address_send_bytes, ADDRESS_SIZE+1, DISABLE_CE);
}

void RFM75_flush_both_rx_tx_fifos(void){
	uint8_t command = FLUSH_TX;
	RC_Iface_send_bytes(&command, 1, DISABLE_CE);
	command = FLUSH_RX;
	RC_Iface_send_bytes(&command, 1, DISABLE_CE);
}

void RFM75_Transmit_bytes(uint8_t *buffer, uint8_t size){
	if(size > 32){return;}
	uint8_t readbytes[2] = {STATUS, 0};
	uint8_t command = W_TX_PAYLOAD;
	RFM75_set_bank(0);
	RC_Iface_send_bytes(&command, 1, LEAVE_CE_ENABLED);
	RC_Iface_send_bytes(buffer, size, DISABLE_CE);
	RC_Iface_toggle_CE();
	rxtx_interrupt = 0;
	while(rxtx_interrupt == 0){}
	rxtx_interrupt = 0;
	RC_Iface_read_bytes(readbytes, 2, DISABLE_CE);
	asm("NOP");
}

void RFM75_set_bank(uint8_t bank){
	if(!((bank == 1) || (bank == 0))){return;}
	uint8_t current_bank;
	current_bank = RFM75_current_bank();
	if(current_bank != bank){
		RFM75_SwitchBank();
	}
}

void RFM75_toggle_reg4_bits(void){
	uint8_t const * const msg_ptr = &bank1_config_rfm[3*5];
	uint8_t write_buffer[5];
	for(uint8_t i=0; i<5; i++){
		write_buffer[i] = msg_ptr[i];
	}
	RFM75_set_bank(1);
	/* Bits 25 and 26 have to be set to 1, then to 0 */
	/*bits        index */
	/*31-24  -> write_buffer[1]*/
	/*23-16 -> write_buffer[2]*/
	/*15-8  -> write_buffer[3]*/
	/*7-0 -> write_buffer[4]*/
	write_buffer[1] |= ((1<<1) | (1<<2));
	RC_Iface_send_bytes(write_buffer, 5, DISABLE_CE);
	_delay_us(100);
	write_buffer[1] &= ~((1<<1) | (1<<2));
	RC_Iface_send_bytes(write_buffer, 5, DISABLE_CE);
}

RFM75_Rx_Status_e RFM75_Receive_bytes(uint8_t *buffer, uint8_t size){
	uint8_t read_buf_cmd = R_RX_PAYLOAD;
	uint8_t static const datasize = DATASIZE_RFM75_TRANSMIT;
	while(rxtx_interrupt == 0){}
	rxtx_interrupt = 0;
	RC_Iface_send_bytes(&read_buf_cmd, 1, LEAVE_CE_ENABLED);
	RC_Iface_read_bytes_no_cmd(buffer, datasize, DISABLE_CE);
	return RFM_RX_SUCCESS;
}

void PinInterruptHandler(void){
	rxtx_interrupt = 1;
	asm("NOP");
}
