/* AUTOGENERATED FILE. DO NOT EDIT. */

//=======Test Runner Used To Run Each Test Below=====
#define RUN_TEST_NO_ARGS
#define RUN_TEST(TestFunc, TestLineNum, ...) \
{ \
  Unity.CurrentTestName = #TestFunc "(" #__VA_ARGS__ ")"; \
  Unity.CurrentTestLineNumber = TestLineNum; \
  Unity.NumberOfTests++; \
  if (TEST_PROTECT()) \
  { \
      setUp(); \
      TestFunc(__VA_ARGS__); \
  } \
  if (TEST_PROTECT() && !TEST_IS_IGNORED) \
  { \
    tearDown(); \
  } \
  UnityConcludeTest(); \
}

//=======Automagically Detected Files To Include=====
#include "unity.h"
#include <setjmp.h>
#include <stdio.h>
#include <setjmp.h>
#include <stdio.h>

//=======External Functions This Runner Calls=====
extern void setUp(void);
extern void tearDown(void);
extern void test_TheseShouldAllPass(int Num);
extern void test_TheseShouldAllFail(int Num);
extern void test_TheseAreEveryOther(int Num);
extern void test_NormalPassesStillWork(void);
extern void test_NormalFailsStillWork(void);


//=======Test Reset Option=====
void resetTest(void);
void resetTest(void)
{
  tearDown();
  setUp();
}


//=======MAIN=====
int main(void)
{
  UnityBegin("tests/testparameterized.c");
  RUN_TEST(test_TheseShouldAllPass, 61, 0);
  RUN_TEST(test_TheseShouldAllPass, 61, 44);
  RUN_TEST(test_TheseShouldAllPass, 61, (90)+9);
  RUN_TEST(test_TheseShouldAllFail, 69, 3);
  RUN_TEST(test_TheseShouldAllFail, 69, 77);
  RUN_TEST(test_TheseShouldAllFail, 69,  (99) + 1 - (1));
  RUN_TEST(test_TheseAreEveryOther, 80, 1);
  RUN_TEST(test_TheseAreEveryOther, 80, 44);
  RUN_TEST(test_TheseAreEveryOther, 80, 99);
  RUN_TEST(test_TheseAreEveryOther, 80, 98);
  RUN_TEST(test_NormalPassesStillWork, 94, RUN_TEST_NO_ARGS);
  RUN_TEST(test_NormalFailsStillWork, 99, RUN_TEST_NO_ARGS);

  return (UnityEnd());
}