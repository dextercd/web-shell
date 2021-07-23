#ifndef PTY_MNGR_FATAL_HPP
#define PTY_MNGR_FATAL_HPP

[[noreturn]]
void fatal_with_error(char const* const errstr, int const err_code);

[[noreturn]]
void fatal(char const* const errstr);

#endif // header guard
