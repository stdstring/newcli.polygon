#ifndef H_FD_HELPER
#define H_FD_HELPER

#include <array>
#include <poll.h>

#define FD_COUNT 2
#define STDIN_INDEX 0
#define SOCKETD_INDEX 1

std::array<struct pollfd, FD_COUNT> create_fdarray(int socketd);

void clear_fdarray(std::array<struct pollfd, FD_COUNT> &fdarray);

#endif