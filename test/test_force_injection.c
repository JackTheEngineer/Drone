#include "test_helper.h"
#include "vector_operations.h"
#include "disturbing_force_injection.h"
#include "vector_tester.h"


TEST_GROUP(force_injection);
POINTER_TO_CONTAINER(Vector_t, force);
POINTER_TO_CONTAINER(Vector_t, moment);

TEST_SETUP(force_injection){
}

TEST_TEAR_DOWN(force_injection){
}

TEST(force_injection, Disturbing_force_should_give_Zero_when_not_initialized){
	POINTER_TO_CONTAINER(Vector_t, compare_vect);
	Vect_set_all_values_to(compare_vect, 0.0);

	Get_error_force_in_earth_frame(force);
	Test_vectors_equal(compare_vect, force);
}

TEST(force_injection, After_setting_disturbing_force_should_return_value){
	POINTER_TO_CONTAINER(Vector_t, injected_vector);

	Vect_set_all_values_to(injected_vector, 5);
	Vect_set_all_values_to(force, 7);

	Inject_Force_into_simulation_in_earth_frame(injected_vector);
	Get_error_force_in_earth_frame(force);
       
	Test_vectors_equal(force, injected_vector);
}


TEST(force_injection, Disturbing_moment_should_give_Zero_when_not_initialized){
	POINTER_TO_CONTAINER(Vector_t, compare_vect);
	Vect_set_all_values_to(compare_vect, 0.0);

	Get_error_moment_in_earth_frame(moment);
	Test_vectors_equal(compare_vect, moment);
}

TEST(force_injection, After_setting_disturbing_moment_should_return_value){
	POINTER_TO_CONTAINER(Vector_t, injected_vector);

	Vect_set_all_values_to(injected_vector, 5);
	Vect_set_all_values_to(moment, 7);

	Inject_Moment_into_simulation_in_earth_frame(injected_vector);
	Get_error_moment_in_earth_frame(moment);
       
	Test_vectors_equal(moment, injected_vector);
}

