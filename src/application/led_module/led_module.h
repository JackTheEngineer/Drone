#ifndef LED_MODULE
#define LED_MODULE

typedef enum lednumbers{
    LED0=8,
    LED1=9,
}leds_t;


void led_on(leds_t led);
void led_off(leds_t led);
void led_toggle(leds_t led);
void leds_init(void);

#endif
