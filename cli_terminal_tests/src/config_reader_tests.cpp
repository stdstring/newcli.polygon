#include <sstream>
#include <vector>
#include <gtest/gtest.h>
#include "config_reader.h"

namespace cli_terminal
{

inline bool operator==(config_entry const &left, config_entry const &right)
{
    return (left.key == right.key) && (left.value == right.value);
}

inline bool operator!=(config_entry const &left, config_entry const &right)
{
    return !(left == right);
}

TEST(ConfigReaderTests, ReadEmptySource)
{
    std::stringstream input("");
    EXPECT_EQ(std::vector<config_entry>(), read_config(input));
}

TEST(ConfigReaderTests, ReadSourceWithWhitespaces)
{
    std::stringstream input(" \t\r\n");
    EXPECT_EQ(std::vector<config_entry>(), read_config(input));
}

TEST(ConfigReaderTests, ReadSourceWithComment)
{
    std::stringstream input("#IDDQD=IDKFA");
    EXPECT_EQ(std::vector<config_entry>(), read_config(input));
}

TEST(ConfigReaderTests, ReadSimpleValue)
{
    std::stringstream input("IDDQD=666");
    std::vector<config_entry> expected = {{"IDDQD", "666"}};
    EXPECT_EQ(expected, read_config(input));
}

TEST(ConfigReaderTests, ReadSimpleValueWithWhiteSpaces)
{
    std::stringstream input(" \tIDDQD = 666 \t");
    std::vector<config_entry> expected = {{"IDDQD ", " 666"}};
    EXPECT_EQ(expected, read_config(input));
}

TEST(ConfigReaderTests, ReadSeveralValues)
{
    std::stringstream input("IDDQD=666\nIDKFA=999");
    std::vector<config_entry> expected = {{"IDDQD", "666"}, {"IDKFA", "999"}};
    EXPECT_EQ(expected, read_config(input));
}

TEST(ConfigReaderTests, ReadSeveralValuesWithCommentsAndWhitespaces)
{
    std::stringstream input("#IMPULSE=9\n \tIDDQD=666 \t\n \tIDKFA = 999 \t\n#some comment");
    std::vector<config_entry> expected = {{"IDDQD", "666"}, {"IDKFA ", " 999"}};
    EXPECT_EQ(expected, read_config(input));
}

TEST(ConfigReaderTests, ReadBadValue)
{
    std::stringstream input("IDDQD");
    EXPECT_EQ(std::vector<config_entry>(), read_config(input));
}

TEST(ConfigReaderTests, ReadSuitableAndBadValues)
{
    std::stringstream input("IDDQD=666\nIMPULSE 9\nIDKFA=999");
    std::vector<config_entry> expected = {{"IDDQD", "666"}, {"IDKFA", "999"}};
    EXPECT_EQ(expected, read_config(input));
}

TEST(ConfigReaderTests, ComplicatedExample)
{
    std::stringstream input("#IMPULSE=9\n \tIDDQD=666 \t\nIMPULSE 9\n \tIDKFA = 999 \t\n#some comment\nIMPULSE 666");
    std::vector<config_entry> expected = {{"IDDQD", "666"}, {"IDKFA ", " 999"}};
    EXPECT_EQ(expected, read_config(input));
}

}