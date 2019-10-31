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
	DIGITAL_IO_SetOutputHigh(&LED1);
	DIGITAL_IO_SetOutputHigh(&LED2);

	bool initialize=0;

	while(initialize == 0){
		delay_ms(25);
		initialize = RFM75_Init();
		DIGITAL_IO_ToggleOutput(&LED1);
	}
	RFM75_setRxModeIfNeeded();
	configRxPipe(/* Pipe number */ 0,
		     address,
		     /* Static = 1, Dynamic = 0 */ 0,
		     /*	Enable Auto Acknowledge */ 1);
	RFM75_setChannel(50);
	CE_HIGH;


	uint8_t received_bytes[32] = {0};
	uint8_t received_length;
	uint32_t remembered_systick_count = 0;

	uint16_t val = 1;
	uint16_t joystick_bytes[4];
	while(1U){
		if(remembered_systick_count != g_systick_count){
			remembered_systick_count = g_systick_count;
			if((remembered_systick_count % 20) == 0){
				received_length = RFM75_Receive_bytes(received_bytes);
				if(received_length != 0){
					DIGITAL_IO_ToggleOutput(&LED2);
					format_u8buf_to_four_ui12(received_bytes, joystick_bytes);
					val = joystick_bytes[0];
					asm("nop");
				}
			}
		}
	}
}

