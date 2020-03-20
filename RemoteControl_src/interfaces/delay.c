#include "delay.h"

extern volatile uint32_t tick_count;

/* Assumes tick count to be 1 tick  = 1 ms */
void _delay_ms(uint32_t ms){
	/* Overflow - Protected diff calculation */
	bool hbs;
	uint32_t dt;
	uint32_t current_ticks = tick_count;
	
	/* Is highest bit set ? */
	hbs = ((tick_count >> 31) & 0x01);
		
	while(true){
		if(hbs && (((tick_count >> 31) & 0x01) == 0)){
			dt = tick_count + (0xFFFFFFFF - current_ticks);
		}else{
			dt = tick_count - current_ticks;
		}
		if(dt > ms){
			break;
		}
	}
}

