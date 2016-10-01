
#include "base.h"
#include "led_module.h"

int main(void){
	leds_init();
	led_toggle(LED0);
	led_toggle(LED1);
	while(true){
	}
	return 0;
}
