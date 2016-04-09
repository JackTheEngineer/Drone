
#include "base.h"
#include "led_module.h"

#define COUNT 1000000

void wait(uint64_t time){
	uint64_t i;
	uint32_t i2;
	for(i=0; i<time;i++){
		while(i2<COUNT){
			i2++;
		}
		i=0;
	}
}

int main(void){

	while(true){

    	led_toggle(LED0);
    	led_toggle(LED1);
    	wait(COUNT);

    }
    return 0;
}
