/*
*/

#ifndef RFM75_RADIO_H
#define RFM75_RADIO_H

#include "delay.h"

bool init();
void RFM75_turnOn();
void RFM75_turnOff();
void RFM75_setPower(uint8_t pwr);
void RFM75_set_RX_mode_if_needed(void);
void RFM75_set_TX_mode_if_needed(void);


#define DBMM10       0x00 // parameter for setPower(pwr): -10 dBm
#define DBMM5        0x01 // parameter for setPower(pwr): -5 dBm
#define DBM0         0x02 // parameter for setPower(pwr): 0 dBm
#define DBM5         0x03 // parameter for setPower(pwr): +5 dBm

typedef enum
{
	SUCCESS = 0,
	FIFOFULL = 1,
	MAXRT = 2,
	UNKNOWN = 3
} TransmitStatus_t;

typedef struct
{
	TransmitStatus_t status;
	uint32_t bytesSent;
} TransmitResult_t;

#define WITH_ACK     0x01 // parameter for sendPayload(..): send with ack expectation
#define NO_ACK       0x00 // parameter for sendPayload(..): send without ack expectation
#define MODE_PRIMARY_TX     0x00 // parameter for setMode(mode): set to transmitter
#define MODE_PRIMARY_RX     0x01 // parameter for setMode(mode): set to receiver
#define EN_AA        0x01 // parameter for configRxPipe(..): enable pipe auto ack
#define NO_AA        0x00 // parameter for configRxPipe(..): disable pipe auto ack
#define TX_DPL       0x01 // parameter for configTxPipe(..): enable dynamic payload for PTX
#define TX_SPL       0x00 // parameter for configTxPipe(..): enable static payload for PTX
#define CRC0         0x00 // parameter for configCRC(crc): disable CRC
#define CRC1         0x01 // parameter for configCRC(crc): 1 byte CRC
#define CRC2         0x02 // parameter for configCRC(crc): 2 byte CRC
#define MBPS1        0x01 // parameter for configSpeed(speed): 1Mbps
#define MBPS2        0x02 // parameter for configSpeed(speed): 2Mbps
#define PWR_BIT      0x02 // parameter for configSpeed(speed): 2Mbps

#define RFM7x_MAX_PACKET_LEN 32// max value is 32
#define RFM7x_BEGIN_INIT_WAIT_MS 0 // pause before Init Registers: 3000ms after RFM73 power up, but Raspberry Pi boots longer than 3s so can be set to 0ms here
#define RFM7x_END_INIT_WAIT_MS 100 // pause after init registers

// SPI commands
#define RFM7x_CMD_READ_REG 0x00 // Define read command to register
#define RFM7x_CMD_WRITE_REG 0x20 // Define write command to register
#define RFM7x_CMD_RD_RX_PLOAD 0x61 // Define RX payload command
#define RFM7x_CMD_WR_TX_PLOAD 0xA0 // Define TX payload command
#define RFM7x_CMD_FLUSH_TX 0xE1 // Define flush TX register command
#define RFM7x_CMD_FLUSH_RX 0xE2 // Define flush RX register command
#define RFM7x_CMD_REUSE_TX_PL 0xE3 // Define reuse TX payload register command
#define RFM7x_CMD_W_TX_PAYLOAD_NOACK 0xb0 // Define TX payload NOACK command
#define RFM7x_CMD_W_ACK_PAYLOAD 0xa8 // Define Write ack command
#define RFM7x_CMD_ACTIVATE 0x50 // Define feature activation command
#define RFM7x_CMD_RX_PL_WID 0x60 // Define received payload width command
#define RFM7x_CMD_NOP_NOP 0xFF // Define No Operation, might be used to read status register

// Interrupts
#define RFM7x_IRQ_STATUS_RX_DR 0x40 // Status bit RX_DR IRQ
#define RFM7x_IRQ_STATUS_TX_DS 0x20 // Status bit TX_DS IRQ
#define RFM7x_IRQ_STATUS_MAX_RT 0x10 // Status bit MAX_RT IRQ
#define RFM7x_IRQ_STATUS_TX_FULL 0x01 
#define RFM7x_PIN_PRIM_RX 0x01
#define RFM7x_PIN_POWER 0x02

// FIFO status
#define RFM7x_FIFO_STATUS_TX_REUSE 0x40
#define RFM7x_FIFO_STATUS_TX_FULL 0x20
#define RFM7x_FIFO_STATUS_TX_EMPTY 0x10
#define RFM7x_FIFO_STATUS_RX_FULL 0x02
#define RFM7x_FIFO_STATUS_RX_EMPTY 0x01

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

// adr has to be 5 bytes!
uint8_t RFM75_configRxPipe(uint8_t pipe_nr, uint8_t *adr, uint8_t plLen, uint8_t en_aa);
void RFM75_enableRxPipe(uint8_t pipe_nr);
void RFM75_SPI_write_buffer_at_start_register(uint8_t cmd, const uint8_t * buf, uint8_t len);
void RFM75_disableRxPipe(uint8_t pipe_nr);

// adr has to be 5 bytes!
void RFM75_configTxPipe(uint8_t * adr, uint8_t pltype);
void RFM75_flushTxFIFO();
void RFM75_flushRxFIFO();
uint8_t RFM75_Receive_bytes(uint8_t *payload);
uint8_t RFM75_sendPayload(const uint8_t * payload, const uint8_t len, const uint8_t toAck);

#endif

