#include "test_helper.h"
#include "matrix_operations.h"
#include "physical_helpers.h"

#define NMBR_MASSES 2
Masspoint_t mps[NMBR_MASSES] ={
    {
        .v = {
            .x = 2.0,
            .y = 1.0,
            .z = 1.0,
        },
        .m = 3.0,
    },
    {
        .v = {
            .x = 2.0,
            .y = 3.0,
            .z = 1.0,
        },
        .m = 5.0,
    }
}; // Inertial moment was hand-calculated once to perform a test on the calculation.
// Don't change these values !

Masspoint_t* masspoints = mps;

TEST_GROUP(physical_helper);
three_by_three_t matrix_container;
three_by_three_t *mom_of_inertia = &matrix_container;

TEST_SETUP(physical_helper){
}

TEST_TEAR_DOWN(physical_helper){
}

TEST(physical_helper, calculation_of_moment_of_interia){
    Mat_set_all_values_to(mom_of_inertia, 0.0);
    physics_calculate_moment_of_inertia(masspoints, NMBR_MASSES, mom_of_inertia);
    TEST_ASSERT_EQUAL_DOUBLE(mom_of_inertia->M[0][0], 56.0);
    TEST_ASSERT_EQUAL_DOUBLE(mom_of_inertia->M[1][0], -36.0);
    TEST_ASSERT_EQUAL_DOUBLE(mom_of_inertia->M[2][0], -16.0);
    TEST_ASSERT_EQUAL_DOUBLE(mom_of_inertia->M[0][1], -36.0);
    TEST_ASSERT_EQUAL_DOUBLE(mom_of_inertia->M[1][1], 40.0);
    TEST_ASSERT_EQUAL_DOUBLE(mom_of_inertia->M[2][1], -18.0);
    TEST_ASSERT_EQUAL_DOUBLE(mom_of_inertia->M[0][2], -16.0);
    TEST_ASSERT_EQUAL_DOUBLE(mom_of_inertia->M[1][2], -18.0);
    TEST_ASSERT_EQUAL_DOUBLE(mom_of_inertia->M[2][2], 80.0);
}
