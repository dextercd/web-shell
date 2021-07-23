#include "fatal.hpp"

#include <cstdlib>
#include <iostream>
#include <cstring>

[[noreturn]]
void fatal_with_error(char const* const errstr, int const err_code)
{
    std::cerr << "fatal error: " << errstr << '\n';

    if (err_code) {
        std::cerr << "errno was set: (";

        auto const error_name = strerrorname_np(err_code);
        auto const error_description = strerrordesc_np(err_code);

        if (error_name)
            std::cerr << error_name << ") ";
        else
            std::cerr << "unknown) ";

        if (error_description)
            std::cerr << error_description << ".\n";
        else
            std::cerr << "No description.\n";
    }

    std::abort();
}

[[noreturn]]
void fatal(char const* const errstr)
{
    auto const err_code = errno;
    fatal_with_error(errstr, err_code);
}
