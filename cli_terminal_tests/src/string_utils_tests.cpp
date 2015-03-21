#include <string>
#include <gtest/gtest.h>
#include "string_utils.h"

namespace cli_terminal
{

TEST(StringUtilsTests, TrimLeft)
{
    ASSERT_EQ(std::string("IDDQD"), trim_left("IDDQD"));
    ASSERT_EQ(std::string("IDDQD"), trim_left(" \t\r\nIDDQD"));
    ASSERT_EQ(std::string("IDDQD \t\r\n"), trim_left("IDDQD \t\r\n"));
    ASSERT_EQ(std::string("IDDQD \t\r\n"), trim_left(" \t\r\nIDDQD \t\r\n"));
    ASSERT_EQ(std::string(""), trim_left(" \t\r\n"));
    ASSERT_EQ(std::string(""), trim_left(""));
    EXPECT_EQ(1, 1);
}

}