#include "motion_sensor.h"
#include "base.h"
#include "hardware.h"
#include "RFM75_driver.h"
#include "byte_formatting.h"


extern volatile uint32_t g_systick_count;


int main(void){
	uint8_t address[5] = {1,0xA,0xF,0xA,0x5};
	(void)DAVE_Init();

	SYSTIMER_Start();
	DIGITAL_IO_SetOutputHigh(&LED1);
	DIGITAL_IO_SetOutputHigh(&LED2);

	bool initialize=0;

	while(initialize == 0){
		delay_ms(25);
		initialize = RFM75_Init();
		DIGITAL_IO_ToggleOutput(&LED1);
	}
	RFM75_set_RX_mode_if_needed();
	RFM75_configRxPipe(/* Pipe number */ 0,
		     address,
		     /* Static = 1, Dynamic = 0 */ 0,
		     /*	Enable Auto Acknowledge */ 1);
	RFM75_setChannel(50);
	RFM75_CE_PIN_high();


	uint8_t received_bytes[32] = {0};
	uint8_t received_length;
	uint16_t rfm_status;
	uint32_t remembered_systick_count = 0;

	uint16_t val = 1;
	uint8_t uart_bytes[3];
	uart_bytes[2] = '\n';
	while(1U){
		if(remembered_systick_count != g_systick_count){
			remembered_systick_count = g_systick_count;
			if((remembered_systick_count % 20) == 0){
				//received_length = RFM75_Receive_bytes(received_bytes);
				rfm_status = RFM75_Receive_bytes_feedback(received_bytes);
				DIGITAL_IO_ToggleOutput(&LED2);
				uart_bytes[0] = rfm_status;
				uart_bytes[1] = (rfm_status >> 8);
				UART_Transmit(&DEBUG_UART, uart_bytes, 3);
			}
		}
	}
}

