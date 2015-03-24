#ifndef H_FD_HELPER
#define H_FD_HELPER

#include <array>
#include <poll.h>

namespace cli_terminal
{

const int fd_count = 2;
const int stdin_index = 0;
const int socketd_index = 1;

std::array<struct pollfd, fd_count> create_fdarray(int socketd);

void clear_fdarray(std::array<struct pollfd, fd_count> &fdarray);

}

#endif