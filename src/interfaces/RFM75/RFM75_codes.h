#ifndef __RFM75_CODES__H__
#define __RFM75_CODES__H__

#define READ_RFM 0x0
#define WRITE_RFM (1<<5)
#define READ_COMMAND_RFM(x) (0x1F & (x))
#define WRITE_COMMAND_RFM(x) ((0x7F) & (WRITE_RFM|(x)))

#define R_RX_PAYLOAD 0x61
#define W_TX_PAYLOAD 0xA0

#define FLUSH_TX 0b11100001
#define FLUSH_RX 0b11100010

#define ACTIVATE 0b01010000
#define ACTIVATE_BYTE 0x73

#define R_RX_PL_WID 0b01100000

#define W_ACK_PAYLOAD (10101 << 3)
#define W_TX_PAYLOAD_NO_ACK 0b10110000

/* 
   Register Bank 0 Adresses, can be used like:
   uint8_t cmd = (WRITE_RFM)|(EN_AA);
   uint8_t value = (ENAA_P0);
*/
#define CONFIG 0x00
#define MASK_RX_DR (1 << 6)
#define MASK_TX_DS (1 << 5)
#define MASK_MAX_RT (1 << 4)
#define EN_CRC (1 << 3)
#define CRCO (1 << 2)
#define PWR_UP (1 << 1)
#define PRIM_RX (1 << 0)

#define EN_AA 0x01
#define ENAA_P5 (1 << 5)
#define ENAA_P4 (1 << 4)
#define ENAA_P3 (1 << 3)
#define ENAA_P2 (1 << 2)
#define ENAA_P1 (1 << 1)
#define ENAA_P0 (1 << 0)

#define EN_RXADDR 0x02
#define ERX_P5 (1 << 5)
#define ERX_P4 (1 << 4)
#define ERX_P3 (1 << 3)
#define ERX_P2 (1 << 2)
#define ERX_P1 (1 << 1)
#define ERX_P0 (1 << 0)

#define SETUP_AW 0x03
#define ADDRESS_WIDTH_3 0b01
#define ADDRESS_WIDTH_4 0b10
#define ADDRESS_WIDTH_5 0b11

#define SETUP_RETR 0x04
#define AUTO_RETRANSMISSION_DELAY(x) ((x) << 4) /* MAX 4 bit, 1LSB waits for 250 uS */
#define AUTO_RETRANSMISSION_COUNT(x) ((x)) /* Max retransmission on fail of AA */

#define RF_CH 0x05
#define RF_CHANNEL(x) ((x)) /* 7 bits max, default 0b10 */
#define RF_CHANNEL_DEFAULT 0b10

/* Reset Settings are 'good' */
#define RF_SETUP 0x06
#define RF_DR_LOW (1<<5) 
#define PLL_LOCK  (1<<4)
#define RF_DR_HIGH (1<<3)
#define RF_PWR(x) (0b110 & ((x)<< 1)) /* Max 2 bit */
#define LNA_HCURR (1 << 0)
	
#define STATUS 0x07
#define RBANK ( 1 << 7)
/* Data Ready RX FIFO interrupt
   Asserted when new data arrives RX FIFO
   Write 1 to clear bit. 
*/	
#define RX_DR (1<<6)

/* Data Sent TX FIFO interrupt
   Asserted when packet transmitted on TX.
   If AUTO_ACK is activated, this bit is set
   high only when ACK is received.
   Write 1 to clear bit. 
 */
#define TX_DS (1 << 5)

/* If Max rt is asserted, it must be cleared to enable further 
   communication, Write 1 to clear bit. Maximum number of restransmits interrupt */
#define MAX_RT (1<<4)
#define RX_PIPE_NO_mask (0b111 < 1)
#define TX_FULL_mask (1<<0)

#define OBSERVE_TX 0x8
#define PLOS_CNT_mask (0b1111 << 4)
#define ARC_CNT_mask (0b1111)

#define CD 0x9
#define CARR_DETECT (1<<0)

#define RX_ADDR_P0 0xA
#define RX_ADDR_P1 0xB
#define RX_ADDR_P2 0xC
#define RX_ADDR_P3 0xD
#define RX_ADDR_P4 0xE
#define RX_ADDR_P5 0xF

/*
   Transmit address. Used for a PTX device
   (LSB byte is written first)
   Set RX_ADDR_P0 equal to this address to
   handle automatic acknowledge if this is a
   PTX device only., 40bytes
*/
#define TX_ADDR 0x10

/* Number of bytes in RX payload in data
   pipe x (1 to 32 bytes).
   0: not used
   1 = 1 byte
   ...
   32 = 32 bytes, 
 */
#define	RX_PW_P0 0x11
#define	RX_PW_P1 0x12
#define	RX_PW_P2 0x13
#define	RX_PW_P3 0x14
#define	RX_PW_P4 0x15
#define	RX_PW_P5 0x16
#define RX_PW_mask (0b11111)

#define FIFO_STATUS 0x17
#define TX_REUSE (1<<6) /* Read Only. Packet is retransmitted whenn CE is high */
#define TX_FULL (1<<5)  /* Read only */
#define TX_EMPTY (1<<4) /* Read only, 1 means empty */
#define RX_FULL (1<<1) /* Read only */
#define RX_EMPTY (1<<0) /* Read only, 1 means empty */

#define DYNPD 0x1C
#define DPL_P5 (1<<5)
#define DPL_P4 (1<<4)
#define DPL_P3 (1<<3)
#define DPL_P2 (1<<2)
#define DPL_P1 (1<<1)
#define DPL_P0 (1<<0)

#define FEATURE 0x1D
#define EN_DPL (1<<2) /* Enables Dynamic Payload Length */
#define EN_ACK_PAY (1<<1) /* Enables Payload with ACK  */
#define EN_DYN_ACK (1<<0) /* Enables the W_TX_PAYLOAD_NOACK command */

#define ADDRESS_SIZE 5

#endif /*  __RFM75_CODES__H__ */
