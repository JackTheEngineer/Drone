/*
*/

#ifndef RFM75_RADIO_H
#define RFM75_RADIO_H

#include "delay.h"
#include "RFM75_codes.h"

typedef struct __attribute__((packed)) addressAndChannel{
	uint8_t address[5];
	uint8_t channel;
}AddressAndChannel_t;

bool RFM75_initRegisters();
void RFM75_turn_on(uint8_t config_mask);
void RFM75_turn_off();
void RFM75_setPower(uint8_t pwr);
void RFM75_set_RX_mode_if_needed(void);
void RFM75_set_TX_mode_if_needed(void);

void RFM75_set_TX_mode(void);
void RFM75_set_RX_mode(void);
uint8_t RFM75_getMode(void);

void RFM75_setChannel(uint8_t cnum);
uint8_t RFM75_getChannel(void);

uint8_t RFM75_SPI_read_reg_value(uint8_t cmd);
void RFM75_SPI_write_reg_val(uint8_t cmd, uint8_t val);
void RFM75_SPI_write_buffer(uint8_t * cmdbuf, uint8_t len);
void RFM75_SPI_read_buffer(uint8_t reg, uint8_t * buf, uint8_t len);
void RFM75_selectBank(uint8_t bank);

void RFM75_reset_interrupts(void);

void RFM75_configRxPipe(uint8_t pipe_nr, const uint8_t *adr, uint8_t plLen, bool enable_auto_ack);
void RFM75_enableRxPipe(uint8_t pipe_nr);
void RFM75_SPI_write_buffer_at_start_register(uint8_t cmd, const uint8_t * buf, uint8_t len);
void RFM75_disableRxPipe(uint8_t pipe_nr);

void RFM75_configTxPipe(const uint8_t * adr, bool en_dynamic_payload);

uint8_t RFM75_Receive_bytes(uint8_t *payload);
CombinedReg_t RFM75_Receive_bytes_feedback(uint8_t *payload);
void RFM75_startListening(const AddressAndChannel_t * address_and_channel);
void RFM75_prepareForTransmission(const AddressAndChannel_t * address_and_channel);

#endif

