#include "motion_sensor.h"
#include "base.h"
#include "hardware.h"
#include "RFM75_driver.h"
#include "byte_formatting.h"


extern volatile uint32_t g_systick_count;


int main(void){
	uint8_t address[5] = {0x35, 0xAF, 0x42, 0x23, 0x99};
	(void)DAVE_Init();

	SYSTIMER_Start();
	bool initialize=0;

	while(initialize == 0){
		delay_ms(25);
		initialize = RFM75_Init();
		DIGITAL_IO_ToggleOutput(&LED1);
	}
	DIGITAL_IO_SetOutputLow(&LED1);
	DIGITAL_IO_SetOutputLow(&LED2);
	RFM75_set_RX_mode();
	RFM75_configRxPipe(0 		/* Pipe number */ ,
		     	 	   address,
					   0, 		/* Static = 1, Dynamic = 0 */
					   true); 	/*	Enable Auto Acknowledge */
	RFM75_setChannel(50);
	RFM75_CE_PIN_high();

	uint8_t received_bytes[32] = {0};
	uint8_t rx_length=0;
	uint32_t remembered_systick_count = 0;
	
#define NUM_UART_BYTES 7
	uint8_t uart_bytes[NUM_UART_BYTES];
	uart_bytes[NUM_UART_BYTES - 1] = '\n';
	uint16_t joystick_bytes[4];

	while(1U){
		if(remembered_systick_count != g_systick_count){
			remembered_systick_count = g_systick_count;
			if((remembered_systick_count % 20) == 0){
				//received_length = RFM75_Receive_bytes(received_bytes);
				rx_length = RFM75_Receive_bytes(received_bytes);
				if(rx_length == 0){
					DIGITAL_IO_ToggleOutput(&LED1);
				}else{
					DIGITAL_IO_ToggleOutput(&LED2);
					format_u8buf_to_four_ui12(received_bytes, joystick_bytes);
					for(uint8_t j=0; j < (NUM_UART_BYTES-1);j++){
						uart_bytes[j] = received_bytes[j];
					}
					UART_Transmit(&DEBUG_UART, uart_bytes, NUM_UART_BYTES);
				}
			}
		}
	}
}
