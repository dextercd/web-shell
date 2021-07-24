#include <algorithm>
#include <iostream>
#include <iterator>
#include <memory>
#include <thread>
#include <string>
#include <cstring>
#include <csignal>
#include <vector>

#include <fcntl.h>
#include <sys/epoll.h>
#include <sys/ioctl.h>
#include <sys/signalfd.h>
#include <sys/wait.h>
#include <termios.h>
#include <unistd.h>

#include "fatal.hpp"

constexpr int input_fd = 0;
constexpr int output_fd = 1;

int register_epoll(int epollfd, int fd, std::uint32_t events)
{
    auto data = epoll_data{};
    data.fd = fd;

    auto event = epoll_event{
        events, data
    };

    return epoll_ctl(epollfd, EPOLL_CTL_ADD, fd, &event);
}

int epoll_fd = -1;
int child_stop_fd = -1;

struct child_process {
    pid_t pid;
    int master_fd;
};
std::vector<child_process> child_processes;


// Tries to read exactly `original_count` bytes.
// Returns -1 if it couldn't read that much data, otherwise returns `original_count`.
ssize_t read_full(int const fd, void* const buf, std::size_t const original_count)
{
    auto count = original_count;
    auto ptr = reinterpret_cast<char*>(buf);

    while (true) {
        if (count == 0)
            return original_count;

        auto const read_result = read(fd, ptr, count);
        if (read_result <= 0)
            return -1;

        count -= read_result;
        ptr += read_result;
    }
}

ssize_t write_full(int const fd, void const* buffer, std::size_t const original_count)
{
    auto count = original_count;
    while (count > 0) {
        auto const wrote = write(fd, buffer, count);
        if (wrote == -1) {
            auto const err = errno;
            if (err == EINTR)
                continue;

            return -1;
        }

        count -= wrote;
        buffer = reinterpret_cast<const char*>(buffer) + wrote;
    }

    return original_count;
}

std::uint32_t read_input_size()
{
    unsigned char bytes[4];
    auto const size_read = read_full(input_fd, bytes, sizeof(bytes));
    if (size_read == -1)
        fatal("stdin read error.");

    return
        std::uint32_t{bytes[0]} << 24 |
        std::uint32_t{bytes[1]} << 16 |
        std::uint32_t{bytes[2]} << 8  |
        std::uint32_t{bytes[3]} << 0;
}

[[noreturn]]
void setup_child(int slave_fd)
{
    // Don't leak parent process errno into child
    errno = 0;

    setsid();
    ioctl(slave_fd, TIOCSCTTY, 0);

    close(0); close(1); close(2);
    dup2(slave_fd, 0); dup2(slave_fd, 1); dup2(slave_fd, 2);
    setenv("TERM", "gdterm", 1);

    std::cout <<
      R"(     xxx          )"  "\n"
      R"(   x x x x        )"  "\n"
      R"( x xx_|_xx x      )"  "\n"
      R"(     \|/          )"  "\n"
      R"(      |/          )"  "\n"
      R"( Web  |  Shell 🍎 )"  "\n"
      R"(▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔)"  "\n\n";

    while(true) {
        if (!std::cin)
            fatal("read error");

        std::cout << "Give password: ";
        std::string given;
        std::getline(std::cin, given);

        if (given == "youshouldnthardcodepasswords :^)")
            break;
    }

#if 0
    char command[] = "fish";

    char* const args[]{
        command,
        nullptr,
    };
#else
    char command[] = "docker";

    char* const args[]{
        command,
        "run", "--rm", "-ti",

        "--user", "31415926:31415926",
        "--workdir", "/home/guest",
        "--hostname", "webshell",

        "-e", "TERM=gdterm",

        "--memory=200M",
        "--cpus=0.5",

        "guest:latest",

        "/bin/bash",

        nullptr,
    };
#endif

    execvp(command, args);
    fatal("exec call failed.");
}

child_process create_pty()
{
    auto const master_fd = open("/dev/ptmx", O_RDWR | O_CLOEXEC | O_NONBLOCK | O_NOCTTY);
    if (master_fd == -1)
        throw std::runtime_error{"Couldn't open /dev/ptmx."};

    if (grantpt(master_fd) == -1)
        throw std::runtime_error{"grantpt failed"};

    if (unlockpt(master_fd) == -1)
        throw std::runtime_error{"unlockpt failed"};

    auto const pts_name = ptsname(master_fd);
    if (pts_name == nullptr)
        throw std::runtime_error{"ptsname failed"};

    auto const slave_fd = open(pts_name, O_RDWR | O_NOCTTY);
    if (slave_fd == -1)
        throw std::runtime_error{"Couldn't open pts."};

    auto const fork_result = fork();

    // child
    if (fork_result == 0) {
        setup_child(slave_fd);
    }

    if (fork_result == -1)
        throw std::runtime_error{"fork failed."};

    // parent
    close(slave_fd);

    auto const winsz = winsize{
        static_cast<unsigned short>(32),
        static_cast<unsigned short>(132),
        0, 0
    };

    ioctl(master_fd, TIOCSWINSZ, &winsz);
    //TODO: handle errors better in this code

    if (register_epoll(epoll_fd, master_fd, EPOLLIN) == -1)
        fatal("Couldn't register master_fd into epoll.");

    child_processes.push_back({fork_result, master_fd});

    return {fork_result, master_fd};
}

void process_input()
{
    auto const input_size = read_input_size();
    std::unique_ptr<unsigned char[]> read_buffer{new unsigned char[input_size]};
    if (read_full(input_fd, read_buffer.get(), input_size) == -1)
        fatal("stdin read error.");

    if (read_buffer[0] == 'c') { // create
        auto [pid, fd] = create_pty();
        char response[] = {
            /* message size    */ 0, 0, 0, 0,

            'r',
            /* response token  */ 0, 0, 0, 0,
            /* file descriptor */ 0, 0, 0, 0,
            /* pid             */ 0, 0, 0, 0};

        std::uint32_t message_size = sizeof(response) - 4;
        response[0] = (message_size >> 24) & 0xff;
        response[1] = (message_size >> 16) & 0xff;
        response[2] = (message_size >> 8)  & 0xff;
        response[3] = (message_size >> 0)  & 0xff;

        std::memcpy(response + 5, read_buffer.get() + 1, 4);
        std::memcpy(response + 9, &fd, 4);
        std::memcpy(response + 13, &pid, 4);

        static_assert(sizeof(fd) == 4);
        static_assert(sizeof(pid) == 4);

        if (write_full(output_fd, response, sizeof(response)) == -1)
            fatal("Couldn't write create response.");;
    } else if (read_buffer[0] == 'w') { // write
        int master_fd;
        std::memcpy(&master_fd, read_buffer.get() + 1, sizeof(master_fd));
        //std::memcpy(&pid, read_buffer.get() + 5, sizeof(pid));
        write_full(master_fd, read_buffer.get() + 9, input_size - 9);
        // No error handling here because the process may have been stopped already
    } else if (read_buffer[0] == 'k') { // kill

        int master_fd;
        pid_t pid;
        std::memcpy(&master_fd, read_buffer.get() + 1, sizeof(master_fd));
        std::memcpy(&pid, read_buffer.get() + 5, sizeof(pid));

        auto child_it = std::find_if(
            std::begin(child_processes),
            std::end(child_processes),
            [=](auto const& proc) {
                return proc.master_fd == master_fd &&
                       proc.pid == pid; });

        if (child_it != std::end(child_processes)) {
            kill(child_it->pid, SIGTERM);
        }
    } else {
        fatal("Unknown message.");
    }
}

void handle_pty(int master_fd)
{
    auto child_it = std::find_if(
        std::begin(child_processes),
        std::end(child_processes),
        [=](auto const& proc) { return proc.master_fd == master_fd; });

    if (child_it == std::end(child_processes)) {
        std::cerr << master_fd << "\r\n";
        for (auto const& ch : child_processes) {
            std::cerr << "  " << ch.pid << " " << ch.master_fd << "\r\n";
        }
        fatal("Couldn't find child process");
    }

    auto constexpr read_size = 1024;
    auto constexpr extra_size = 13;
    char buffer[extra_size + read_size];
    buffer[4] = 'o';
    std::memcpy(buffer + 5, &master_fd, 4);
    std::memcpy(buffer + 9, &child_it->pid, 4);

    ssize_t s;
    std::this_thread::sleep_for(std::chrono::milliseconds{1});
    while((s = read(master_fd, buffer + extra_size, read_size)) > 0) {
        std::uint32_t message_size = s + extra_size - 4;
        buffer[0] = (message_size >> 24) & 0xff;
        buffer[1] = (message_size >> 16) & 0xff;
        buffer[2] = (message_size >> 8)  & 0xff;
        buffer[3] = (message_size >> 0)  & 0xff;
        if (write_full(output_fd, buffer, message_size + 4) == -1)
            fatal("Couldn't write terminal output.");

    }
}

void notify_child_stop(child_process process, int status)
{
    char response[] = {
        /* message size    */ 0, 0, 0, 13,

        'x',
        /* file descriptor */ 0, 0, 0, 0,
        /* pid             */ 0, 0, 0, 0,
        /* status          */ 0, 0, 0, 0};

    std::memcpy(response + 5, &process.master_fd, sizeof(process.master_fd));
    std::memcpy(response + 9, &process.pid, sizeof(process.pid));
    response[13] = (status >> 24) & 0xff;
    response[14] = (status >> 16) & 0xff;
    response[15] = (status >> 8)  & 0xff;
    response[16] = (status >> 0)  & 0xff;

    if (write_full(output_fd, response, sizeof(response)) == -1)
        fatal("Couldn't write terminal exit notification.");
}

void process_child_stops()
{
    // child_stop_fd may not contain all signals because some may be lost due to
    // 'coalescing' same signal types. Instead we just discard the child_stop_fd
    // and use the waitpid interface.
    char discard[1024];
    while(read(child_stop_fd, discard, sizeof(discard)) > 0);

    while (true) {
        int status = -1;
        auto const pid = waitpid(-1, &status, WNOHANG);

        if (pid == 0)
            break;

        if (pid == -1) {
            if (errno == EINTR)
                continue;

            if (errno == ECHILD)
                break;

            fatal("waitpid failed.");
        }

        auto child_it = std::find_if(
            std::begin(child_processes),
            std::end(child_processes),
            [=](auto const& proc) { return proc.pid == pid; });

        if (child_it == std::end(child_processes)) {
            std::cerr << pid << '\n';
            fatal("Couldn't find child process.");
        }

        handle_pty(child_it->master_fd);

        close(child_it->master_fd);

        notify_child_stop(*child_it, status);

        child_processes.erase(child_it);
    }
}

int main()
{
    epoll_fd = epoll_create1(EPOLL_CLOEXEC);
    if (epoll_fd == -1)
        fatal("Couldn't open epoll file descriptor.");

    if (register_epoll(epoll_fd, input_fd, EPOLLIN) == -1)
        fatal("Couldn't register input_fd into epoll.");

    sigset_t mask;
    sigemptyset(&mask);
    sigaddset(&mask, SIGCHLD);

    if (sigprocmask(SIG_BLOCK, &mask, nullptr) == -1)
        fatal("Couldn't block signals.");

    child_stop_fd = signalfd(-1, &mask, SFD_NONBLOCK | SFD_CLOEXEC);
    if (child_stop_fd == -1)
        fatal("signalfd failed.");

    if (register_epoll(epoll_fd, child_stop_fd, EPOLLIN) == -1)
        fatal("Couldn't register child_stop_fd into epoll.");

    while(true) {
        epoll_event triggered_events[10];
        auto const wait_result = epoll_wait(
            epoll_fd, triggered_events, std::size(triggered_events), -1);

        if (wait_result == -1) {
            auto const err = errno;
            if (err == EINTR) {
                // Interrupts happen, just move on in that case
                continue;
            }

            fatal_with_error("epoll_wait failed.", err);
        }

        for (auto i = 0; i != wait_result; ++i) {
            auto const& current_event = triggered_events[i];

            if (current_event.data.fd == input_fd) {
                process_input();
            } else if (current_event.data.fd == child_stop_fd) {
                process_child_stops();
            } else {
                handle_pty(current_event.data.fd);
            }
        }
    }
}
