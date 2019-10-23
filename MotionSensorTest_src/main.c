#include "motion_sensor.h"
#include "base.h"
#include "hardware.h"


uint32_t volatile g_systick_count;


int main(void){
	POINTER_TO_CONTAINER(Sensordata_t, motion_sensor);
	DAVE_STATUS_t status = DAVE_Init();

	SYSTIMER_Start();
	DIGITAL_IO_SetOutputHigh(&LED1);
	DIGITAL_IO_SetOutputHigh(&LED2);
	Motion_sensor_init(motion_sensor);

	while(1U){
		if((g_systick_count % 100) == 0){
			Motion_sensor_get_data(motion_sensor);
		}
	}
}
