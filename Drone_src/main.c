#include "motion_sensor.h"
#include "motor_pwm.h"
#include "timetasks.h"
#include "led_module.h"
#include "buttons.h"
#include "os.h"
#include "delay.h"
#include "RFM75_driver.h"
#include "statemachine.h"
#include "uart.h"
#include "byte_manip.h"


#include "digital_io.h"
const DIGITAL_IO_t DBG_PIN =
{
  .gpio_port = XMC_GPIO_PORT1,
  .gpio_pin = 11U,
  .gpio_config = {
    .mode = XMC_GPIO_MODE_OUTPUT_PUSH_PULL,
    .output_level = XMC_GPIO_OUTPUT_LEVEL_LOW,

  },
  .hwctrl = XMC_GPIO_HWCTRL_DISABLED
};

uint32_t volatile tick_count;
extern void SysTick_Handler(void){
	tick_count++;
}

bool UpdateTime(uint32_t *last_ticks){
	if(tick_count != *last_ticks){
		*last_ticks = tick_count;
		return true;
	}else{
		return false;
	}
}

int main(void)
{
	uint32_t last_ticks = 0;
	POINTER_TO_CONTAINER(OS_t, os);
	POINTER_TO_CONTAINER(Sensordata_t, motion_sensor);
	State_t state = STATE_CALIBRATE;
	Button_t btn1 = {
			.btn = BUTTON1,
			.laststate = false,
	};

	Button_t btn2 = {
			.btn = BUTTON2,
			.laststate = false,
	};

	os->motion_sensor = motion_sensor;
	os->button_1 = &btn1;
	os->button_2 = &btn2;
	os->current_state = &state;

	PWM_Init();
	PWM_Motor_Set_Rate(0, 0);
	PWM_Motor_Set_Rate(0, 1);
	PWM_Motor_Set_Rate(0, 2);
	PWM_Motor_Set_Rate(0, 3);

	DIGITAL_IO_Init(&DBG_PIN);
	SysTick_Config(SystemCoreClock/1000); /* 1 ms Tick */
	DelayTimer_Init();

	Motion_sensor_init(os->motion_sensor);
	buttons_init();

	bool initialize = false;
	while(initialize == false){
		delay_ms(25);
		initialize = RFM75_Init();
		DIGITAL_IO_ToggleOutput(&LED1);
	}

	delay_ms(100);
	DIGITAL_IO_SetOutputLow(&LED1);

	while(1U){
		if(UpdateTime(&last_ticks)){
			Statemachine_do(last_ticks, os);
		}
	}
	return 1;
}
