#ifndef PTY_MNGR_UNREACHABLE_HPP
#define PTY_MNGR_UNREACHABLE_HPP

#ifdef NDEBUG
    // Release
    #if defined(__GNUC__) || defined(__clang__)
        // Clang and GCC (also matches Intel compilers)
        #define PTY_MNGR_UNREACHABLE __builtin_unreachable()
    #else
        // Other compilers (primarily VC++)
        #include <cstdlib>
        #define PTY_MNGR_UNREACHABLE std::abort()
    #endif
#else
    // Debug
    #include <cassert>
    #define PTY_MNGR_UNREACHABLE assert(false && "Unreachable code hit!")
#endif

#endif // header guard
