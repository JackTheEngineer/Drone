$TESTS = {
  "test_pid_controller" => [
    "pid_controller",
  ],
  "test_matrix_operations" =>[
    "matrix_operations",
    "vector_operations",
    "matrix_tester",
  ],
  "test_physical_helpers" => [
    "physical_helpers",
    "matrix_operations",
    "vector_operations"
  ],
  "test_vector_operations" => [
    "vector_operations",
    "vector_tester"
  ],
  "test_propeller" => [
    "propeller",
  ],
  "test_simulation" => [
    "drone_physics",
    "drone_masses",
    "fake_motors",
    "vector_operations",
    "physical_helpers",
    "matrix_operations",
    "disturbing_force_injection",
  ],
  "test_fake_motors"=>[
    "fake_motors",
    "vector_operations",
    "fake_drone_physics",
  ],
  "test_fake_motion_sensor"=>[
    "vector_operations",
    "fake_motion_sensor",
    "fake_drone_physics",
    "vector_tester",
  ],
  "test_drone_physics" => [
    "drone_physics",
    "drone_masses",
    "vector_operations",
    "vector_tester",
    "matrix_operations",
    "physical_helpers",
    "matrix_tester",
    "disturbing_force_injection",
  ],
  "test_force_injection"=>[
    "disturbing_force_injection",
    "vector_tester",
    "vector_operations",
  ],
}
