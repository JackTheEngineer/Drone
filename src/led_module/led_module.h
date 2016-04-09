#ifndef LED_MODULE
#define LED_MODULE

typedef enum lednumbers{
    LED0,
    LED1
}leds_t;

void led_toggle(leds_t led);

#endif
