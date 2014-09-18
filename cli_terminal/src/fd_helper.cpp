#include <array>

#include <poll.h>
#include <unistd.h>

#include "fd_helper.h"

namespace cli_terminal
{

std::array<struct pollfd, FD_COUNT> create_fdarray(int socketd)
{
    std::array<struct pollfd, FD_COUNT> fdarray;
    fdarray[STDIN_INDEX].fd = STDIN_FILENO;
    fdarray[STDIN_INDEX].events = POLLIN;
    fdarray[SOCKETD_INDEX].fd = socketd;
    fdarray[SOCKETD_INDEX].events = POLLIN;
    return fdarray;
}

void clear_fdarray(std::array<struct pollfd, FD_COUNT> &fdarray)
{
    fdarray[STDIN_INDEX].revents = 0;
    fdarray[SOCKETD_INDEX].revents = 0;
}

}