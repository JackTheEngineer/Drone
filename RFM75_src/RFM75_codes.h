#ifndef __RFM75_CODES__H__
#define __RFM75_CODES__H__

#define MODE_PRIMARY_TX     0x00 // parameter for setMode(mode): set to transmitter
#define MODE_PRIMARY_RX     0x01 // parameter for setMode(mode): set to receiver

#define READ_RX_PAYLOAD 0x61
#define WRITE_TX_PAYLOAD 0xA0
#define RFM7x_CMD_REUSE_TX_PL 0xE3 // Define reuse TX payload register command
#define RFM7x_CMD_W_TX_PAYLOAD_NOACK 0xb0 // Define TX payload NOACK command
#define RFM7x_CMD_W_ACK_PAYLOAD 0xa8 // Define Write ack command
#define READ_PAYLOAD_LENGTH 0x60 // Define received payload width command

#define WRITE_RFM (1<<5)
#define WRITE_COMMAND_RFM(x) ((0x7F) & (WRITE_RFM|(x)))

#define FLUSH_TX 0b11100001
#define FLUSH_RX 0b11100010

#define ACTIVATE 0x50
#define ACTIVATE_BYTE 0x73

#define R_RX_PL_WID 0b01100000

#define W_ACK_PAYLOAD (10101 << 3)
#define W_TX_PAYLOAD_NO_ACK 0b10110000

/* 
   Register Bank 0 Adresses, can be used like:
   uint8_t cmd = (WRITE_RFM)|(EN_AA);
   uint8_t value = (ENAA_P0);
*/
#define CONFIG_REG 0x00
#define HIDE_DATA_RECEIVED_INTERRUPT (1 << 6)
#define HIDE_TX_DATA_SENT_INTERRUPT (1 << 5)
#define HIDE_MAX_RETRANSMITS_INTERRUPT (1 << 4)
#define EN_CRC (1 << 3)
#define CRCO (1 << 2)
#define PWR_UP (1 << 1)
#define PRIM_RX (1 << 0)

typedef union __attribute__((packed, aligned(1))) configReg{
	struct{
		uint8_t primary_rx:1;
		uint8_t power_up:1;
		uint8_t crc_encoding_scheme:1; // 0 = 1 byte, 1 = 2 byte
		uint8_t hide_max_rt_interrupt:1;
		uint8_t hide_tx_data_sent_interrupt:1;
		uint8_t hide_rx_data_ready_interrupt:1;
		uint8_t _reserved;
	};
	uint8_t all;
}ConfigReg_t;


#define PIPE_5 (1 << 5)
#define PIPE_4 (1 << 4)
#define PIPE_3 (1 << 3)
#define PIPE_2 (1 << 2)
#define PIPE_1 (1 << 1)
#define PIPE_0 (1 << 0)
#define ALL_6_PIPES (PIPE_0 | PIPE_1 | PIPE_2 | PIPE_3 | PIPE_4 | PIPE_5 )

#define ENABLE_AUTO_ACK_REG 0x01

#define ENABLE_RX_ADDR_REG 0x02


#define SETUP_ADDR_WIDTH 0x03
#define ADDRESS_WIDTH_3 0b01
#define ADDRESS_WIDTH_4 0b10
#define ADDRESS_WIDTH_5 0b11

#define SETUP_RETR 0x04
#define AUTO_RETR_DELAY(x) (0xF0 & ((x) << 4)) /* MAX 4 bit, 1LSB waits for 250 uS */
#define AUTO_RETR_COUNT(x) ((x)) /* Max retransmission on fail of AUTO_ACK */

#define RF_CH_REG 0x05
#define RF_CHANNEL(x) ((0x7F) & (x)) /* 7 bits max, default 0b10 */
#define RF_CHANNEL_DEFAULT 0b10

/* Reset Settings are 'good' */
#define RF_SETTINGS_REG 0x06
#define RF_DR_LOW (1 << 5)
#define PLL_LOCK  (1 << 4)
#define RF_DR_HIGH (1 << 3)
#define RF_PWR(x) (0x6 & ((x) << 1)) /* Max 2 bit */
#define LNA_HCURR (1 << 0)
typedef union __attribute__((packed, aligned(1))) rfSetupReg{
	struct{
		uint8_t lna_hcurr:1;
		uint8_t rf_power:2;
		uint8_t rf_data_rate_high_bit:1;
		uint8_t _pll_lock:1;
		uint8_t rf_data_rate_low_bit:1;
		uint8_t _reserved:2;
	};
	uint8_t all;
}rfSetupReg_t;
	
#define STATUS_REG 0x07
#define RBANK ( 1 << 7)
/* Data Ready RX FIFO interrupt
   Asserted when new data arrives RX FIFO
   Write 1 to clear bit. 
*/	
#define RX_DATA_READY_FLAG (1<<6)

/* Data Sent TX FIFO interrupt
   Asserted when packet transmitted on TX.
   If AUTO_ACK is activated, this bit is set
   high only when ACK is received.
   Write 1 to clear bit. 
 */
#define TX_DATA_SENT_FLAG (1 << 5)

/* If Max rt is asserted, it must be cleared to enable further 
   communication, Write 1 to clear bit. Maximum number of restransmits interrupt */
#define MAX_RETRANSMITS_FLAG (1<<4)
#define RX_PIPE_NO_mask (0b111 < 1)
#define TX_FULL_mask (1<<0)

typedef union __attribute__((packed, aligned(1))) statusReg{
	struct{
		uint8_t tx_fifo_full:1;
		uint8_t rx_pipe_num:3; /* 0b111 means RX FIFO is FUll */
		uint8_t max_retransmits:1;
		uint8_t tx_data_sent:1;
		uint8_t rx_data_ready:1;
		uint8_t rbank:1;
	};
	uint8_t all;
}StatusReg_t;

#define OBSERVE_TX_REG 0x8
#define PLOS_CNT_mask (0xF0)
#define ARC_CNT_mask (0xF)
typedef union __attribute__((packed, aligned(1))) observeTxReg{
	struct{
		uint8_t retransmit_cnt:4;
		uint8_t packet_lost_cnt:4;
	};
	uint8_t all;
}ObserveTxReg_t;

#define CARRIER_DETECT_REG 0x9
#define CARR_DETECT (1<<0)


#define RX_PIPE_0_ADDR_REG 0xA
#define RX_PIPE_1_ADDR_REG 0xB
#define RX_PIPE_2_ADDR_REG 0xC
#define RX_PIPE_3_ADDR_REG 0xD
#define RX_PIPE_4_ADDR_REG 0xE
#define RX_PIPE_5_ADDR_REG 0xF

/*
   Transmit address. Used for a PTX device
   (LSB byte is written first)
   Set RX_ADDR_P0 equal to this address to
   handle automatic acknowledge if this is a
   PTX device only., 40bytes
*/
#define TX_ADDR_REG 0x10

/* Number of bytes in RX payload in data
   pipe x (1 to 32 bytes).
   0: not used
   1 = 1 byte
   ...
   32 = 32 bytes, 
 */
#define	RX_PIPE_0_PAYLOAD_LENGTH_REG 0x11
#define	RX_PIPE_1_PAYLOAD_LENGTH_REG 0x12
#define	RX_PIPE_2_PAYLOAD_LENGTH_REG 0x13
#define	RX_PIPE_3_PAYLOAD_LENGTH_REG 0x14
#define	RX_PIPE_4_PAYLOAD_LENGTH_REG 0x15
#define	RX_PIPE_5_PAYLOAD_LENGTH_REG 0x16
#define RX_PAYLOAD_LENGTH_MASK (0b11111)

#define FIFO_STATUS_REG 0x17
#define TX_REUSE (1<<6) /* Read Only. Packet is retransmitted whenn CE is high */
#define TX_FULL (1<<5)  /* Read only */
#define TX_EMPTY (1<<4) /* Read only, 1 means empty */
#define RX_FULL (1<<1)  /* Read only */
#define RX_EMPTY (1<<0) /* Read only, 1 means empty */

typedef union __attribute__((packed, aligned(1))) statusRegFifo{
	struct{
		uint8_t rx_empty:1;
		uint8_t rx_full:1;
		uint8_t _reserved:2;
		uint8_t tx_empty:1;
		uint8_t tx_full:1;
		uint8_t tx_reuse:1;
	};
	uint8_t all;
}FifoStatusReg_t;

typedef union __attribute__((packed, aligned(1))) CombinedReg{
	struct{
		uint32_t tx_fifo_full:1;
		uint32_t rx_pipe_num:3; /* 0b111 means RX FIFO is FUll */
		uint32_t max_retransmits:1;
		uint32_t tx_data_sent:1;
		uint32_t rx_data_ready:1;
		uint32_t rbank:1;
		uint32_t retransmit_cnt:4;
		uint32_t packet_lost_cnt:4;
		uint32_t rx_empty:1;
		uint32_t rx_full:1;
		uint32_t _reserved1:2;
		uint32_t tx_empty:1;
		uint32_t tx_full:1;
		uint32_t tx_reuse:1;
		uint32_t _reserved2:1;
		uint32_t length:8;
	};
	uint32_t all;
}CombinedReg_t;

#define DYNAMIC_PAYLOAD_LENGTH_REG 0x1C

#define FEATURE_REG 0x1D
#define EN_DYNAMIC_PAYLOAD_LENGTH (1<<2) /* Enables Dynamic Payload Length */
#define EN_PAYLOAD_WITH_ACK (1<<1) /* Enables Payload with ACK  */
#define EN_SPECIAL_NOACK_COMMAND (1<<0) /* Enables the W_TX_PAYLOAD_NOACK command */

#define ADDRESS_SIZE 5

#endif /*  __RFM75_CODES__H__ */
