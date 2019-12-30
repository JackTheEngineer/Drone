#include "motion_sensor.h"
#include "base.h"
#include "hardware.h"
#include "RFM75_driver.h"
#include "byte_manip.h"

extern const AddressAndChannel_t default_RFM75_Addr;

uint32_t volatile tick_count;

extern void SysTick_Handler(void){
	tick_count++;
}

int main(void){
	uint8_t sendbytes[32] = {0};
	sendbytes[31] = 11;
	sendbytes[0] = 11;
	sendbytes[1] = 22;
	sendbytes[2] = 33;
	sendbytes[3] = 44;

	(void)DAVE_Init();
	SysTick_Config(SystemCoreClock/1000);

	bool initialize = false;
	while(initialize == 0){
		_delay_ms(25);
		initialize = RFM75_Init();
		DIGITAL_IO_ToggleOutput(&LED1);
	}
	RFM75_startListening(&default_RFM75_Addr);

	DIGITAL_IO_SetOutputLow(&LED1);
	DIGITAL_IO_SetOutputLow(&LED2);

	uint8_t received_bytes[32] = {0};
	CombinedReg_t creg;
	uint32_t remembered_systick_count = 0;
	
#define NUM_UART_BYTES 4 + 1
	uint8_t uart_bytes[NUM_UART_BYTES];
	uart_bytes[NUM_UART_BYTES - 1] = '\n';
	uint16_t joystick_bytes[4];

	while(1U){
		if(remembered_systick_count != tick_count){
			remembered_systick_count = tick_count;
			if((remembered_systick_count % 20) == 0){
				//received_length = RFM75_Receive_bytes(received_bytes);
				RFM75_SPI_write_buffer_at_start_register(W_ACK_PAYLOAD(0), sendbytes, 32);
				creg = RFM75_Receive_bytes_feedback(received_bytes);
				if(creg.length == 0){
					DIGITAL_IO_SetOutputHigh(&LED1);
					DIGITAL_IO_SetOutputLow(&LED2);
				}else{
					DIGITAL_IO_ToggleOutput(&LED2);
					DIGITAL_IO_SetOutputLow(&LED1);
				}
				format_u32_to_u8buf(creg.all, uart_bytes);
			}
		}
	}
}
