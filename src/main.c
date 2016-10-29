
#include "base.h"
#include "led_module.h"

int main(void){
	leds_init();
	PWM_Init();
	while(true){
	}
	return 0;
}
