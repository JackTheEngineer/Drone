#ifndef LED_MODULE
#define LED_MODULE

typedef enum lednumbers{
	_LED1=9,
    _LED2=8,
}leds_t;


void gpio_on(leds_t led);
void gpio_off(leds_t led);
void gpio_toggle(leds_t led);
void gpio_init(void);

#endif
