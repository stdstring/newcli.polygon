#include <string>
#include <gtest/gtest.h>
#include "cterm_ptr.h"
#include "string_utils.h"

namespace cli_terminal
{

TEST(StringUtilsTests, TrimLeft)
{
    EXPECT_EQ("IDDQD", trim_left("IDDQD"));
    EXPECT_EQ("IDDQD", trim_left(" \t\r\nIDDQD"));
    EXPECT_EQ("IDDQD \t\r\n", trim_left("IDDQD \t\r\n"));
    EXPECT_EQ("IDDQD \t\r\n", trim_left(" \t\r\nIDDQD \t\r\n"));
    EXPECT_EQ("", trim_left(" \t\r\n"));
    EXPECT_EQ("", trim_left(""));
}

TEST(StringUtilsTests, TrimRight)
{
    EXPECT_EQ("IDDQD", trim_right("IDDQD"));
    EXPECT_EQ(" \t\r\nIDDQD", trim_right(" \t\r\nIDDQD"));
    EXPECT_EQ("IDDQD", trim_right("IDDQD \t\r\n"));
    EXPECT_EQ(" \t\r\nIDDQD", trim_right(" \t\r\nIDDQD \t\r\n"));
    EXPECT_EQ("", trim_right(" \t\r\n"));
    EXPECT_EQ("", trim_right(""));
}

TEST(StringUtilsTests, TrimFull)
{
    EXPECT_EQ("IDDQD", trim_full("IDDQD"));
    EXPECT_EQ("IDDQD", trim_full(" \t\r\nIDDQD"));
    EXPECT_EQ("IDDQD", trim_full("IDDQD \t\r\n"));
    EXPECT_EQ("IDDQD", trim_full(" \t\r\nIDDQD \t\r\n"));
    EXPECT_EQ("", trim_full(" \t\r\n"));
    EXPECT_EQ("", trim_full(""));
}

TEST(StringUtilsTests, Join)
{
    EXPECT_EQ("IDDQD", join({"IDDQD"}, ""));
    EXPECT_EQ("IDDQD", join({"IDDQD"}, "-"));
    EXPECT_EQ("IDDQDIDKFA", join({"IDDQD", "IDKFA"}, ""));
    EXPECT_EQ("IDDQD-IDKFA", join({"IDDQD", "IDKFA"}, "-"));
    EXPECT_EQ("", join({""}, ""));
    EXPECT_EQ("", join({""}, "-"));
    EXPECT_EQ("", join({}, ""));
    EXPECT_EQ("", join({}, "-"));
}

TEST(StringUtilsTests, DuplicateCStr)
{
    std::string source("IDDQD");
    cterm_ptr<char> clone(duplicate_cstr(source));
    EXPECT_STREQ(source.c_str(), clone.get());
    EXPECT_NE(source.c_str(), clone.get());
}

}