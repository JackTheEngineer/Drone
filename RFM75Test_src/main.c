#include "motion_sensor.h"
#include "base.h"
#include "hardware.h"
#include "RFM75_driver.h"
#include "byte_formatting.h"


extern volatile uint32_t g_systick_count;


int main(void){
	uint8_t address[5] = {1,0,0,0,0};
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
	uint8_t creg;
	uint32_t remembered_systick_count = 0;

	uint8_t uart_bytes[5];
	uart_bytes[4] = '\n';
	while(1U){
		if(remembered_systick_count != g_systick_count){
			remembered_systick_count = g_systick_count;
			if((remembered_systick_count % 20) == 0){
				//received_length = RFM75_Receive_bytes(received_bytes);
				creg = RFM75_Receive_bytes(received_bytes);
				if(creg == 0){
					DIGITAL_IO_ToggleOutput(&LED1);
				}else{
					DIGITAL_IO_ToggleOutput(&LED2);
				}
				//format_u32_to_u8buf(creg, uart_bytes);
				UART_Transmit(&DEBUG_UART, uart_bytes, 5);
			}
		}
	}
}