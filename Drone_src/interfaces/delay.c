#include "delay.h"

extern volatile uint32_t g_systick_count;

/* Assumes tick count to be 1 tick  = 1 ms */
void delay_ms(uint32_t ms){
	/* Overflow - Protected diff calculation */
	bool hbs;
	uint32_t dt;
	uint32_t current_ticks = g_systick_count;
	
	/* Is highest bit set ? */
	hbs = ((g_systick_count >> 31) & 0x01);
		
	while(true){
		if(hbs && (((g_systick_count >> 31) & 0x01) == 0)){
			dt = g_systick_count + (0xFFFFFFFF - current_ticks);
		}else{
			dt = g_systick_count - current_ticks;
		}
		if(dt > ms){
			break;
		}
	}
}
