#include <iostream>
#include <math.h>
#include <string>
#include <fstream>
#include <vector>
#define BOOST_TEST_MODULE test utils
#include <boost/test/included/unit_test.hpp>
#include "utils.h"

BOOST_AUTO_TEST_CASE(first_test)
{
  int i = 1;
  BOOST_TEST(i);
  BOOST_TEST(i == 2);
}