
#include "../../application/led_module/led_module.h"

#include "xmc_gpio.h"

const XMC_GPIO_CONFIG_t LED_conf = {
		.mode = XMC_GPIO_MODE_OUTPUT_PUSH_PULL,
		.output_level = XMC_GPIO_OUTPUT_LEVEL_LOW,
		.output_strength = XMC_GPIO_OUTPUT_STRENGTH_STRONG_SOFT_EDGE,
};

void leds_init(void){
	XMC_GPIO_Init(XMC_GPIO_PORT1, 0, &LED_conf);
	XMC_GPIO_Init(XMC_GPIO_PORT1, 1, &LED_conf);
}

void led_toggle(leds_t led){
    XMC_GPIO_ToggleOutput(XMC_GPIO_PORT1, led);
}
