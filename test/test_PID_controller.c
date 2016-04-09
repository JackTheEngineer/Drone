#include "01_scheduler.h"
#include "test_helper.h"
#include "fake_task.h"

TEST_GROUP(01_scheduler);
// Event_Type_t event;
// Event_Type_t* ptr_event = &event;

TEST_SETUP(01_scheduler){
}

TEST_TEAR_DOWN(01_scheduler){
} 

// @todo ask about return bool policy
TEST(01_scheduler, Scheduler_should_return_true_when_okay){
    bool status = SCH_run();

    TEST_ASSERT_TRUE(status);
}
