#include "test_helper.h"
#include "01_task_control.h"

static Event_Type_t fake_current_event;

bool Task_Get_Event_From_Buffer(Event_Type_t* p_Received_Event_e ){
  *p_Received_Event_e = fake_current_event;
  return 1;
}

void Fake_Task_Set_Event(Event_Type_t event){
    fake_current_event = event;
}
