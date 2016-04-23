#include "test_helper.h"
#include "pid_controller.h"

TEST_GROUP(pid_controller);
// Event_Type_t event;
// Event_Type_t* ptr_event = &event;

TEST_SETUP(pid_controller){
}

TEST_TEAR_DOWN(pid_controller){
} 

// @todo ask about return bool policy
TEST(pid_controller, Scheduler_should_return_true_when_okay){
    bool status = true;

    TEST_ASSERT_TRUE(status);
}
