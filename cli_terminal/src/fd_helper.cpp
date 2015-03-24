#include <array>

#include <poll.h>
#include <unistd.h>

#include "fd_helper.h"

namespace cli_terminal
{

std::array<struct pollfd, fd_count> create_fdarray(int socketd)
{
    std::array<struct pollfd, fd_count> fdarray;
    fdarray[stdin_index].fd = STDIN_FILENO;
    fdarray[stdin_index].events = POLLIN;
    fdarray[socketd_index].fd = socketd;
    fdarray[socketd_index].events = POLLIN;
    return fdarray;
}

void clear_fdarray(std::array<struct pollfd, fd_count> &fdarray)
{
    fdarray[stdin_index].revents = 0;
    fdarray[socketd_index].revents = 0;
}

}