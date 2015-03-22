#include <gtest/gtest.h>
#include "base64.h"

namespace cli_terminal
{

TEST(Base64Tests, ToBase64)
{
    EXPECT_EQ("", to_base64(""));
    EXPECT_EQ("YQ==", to_base64("a"));
    EXPECT_EQ("YWI=", to_base64("ab"));
    EXPECT_EQ("YWJj", to_base64("abc"));
    EXPECT_EQ("YWJjZA==", to_base64("abcd"));
}

TEST(Base64Tests, FromBase64)
{
    EXPECT_EQ("", from_base64(""));
    EXPECT_EQ("a", from_base64("YQ=="));
    EXPECT_EQ("ab", from_base64("YWI="));
    EXPECT_EQ("abc", from_base64("YWJj"));
    EXPECT_EQ("abcd", from_base64("YWJjZA=="));
}

}