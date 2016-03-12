
// ================================================================================================
// -*- C++ -*-
// File: output.hpp
// Author: Guilherme R. Lampert
// Created on: 03/03/16
// Brief: Runtime output printing helpers.
// ================================================================================================

#ifndef MOON_OUTPUT_HPP
#define MOON_OUTPUT_HPP

#include <iostream>
#include <string>
#include <cstdio>

// ========================================================
// isatty() only needed if colored text output is desired.
// ========================================================

#ifdef MOON_PRINT_USE_ANSI_COLOR_CODES
    // For isatty()/fileno()
    #if defined(__APPLE__) || defined(__linux__) || defined(__unix__)
        #include <unistd.h>
    #elif defined(_WIN32) || defined(_MSC_VER)
        #include <io.h>
        // Damn Windows with your silly underscores...
        #ifndef isatty
            #define isatty _isatty
        #endif // isatty
        #ifndef fileno
            #define fileno _fileno
        #endif // fileno
    #endif // Apple/Win/Linux
#endif // MOON_PRINT_USE_ANSI_COLOR_CODES

namespace moon
{

// ========================================================
// Colored text printing on the terminal:
// ========================================================

namespace color
{

inline bool colorPrintEnabled() noexcept
{
// Only attempt color print if none of the streams we use were redirected.
#ifdef MOON_PRINT_USE_ANSI_COLOR_CODES
    return isatty(fileno(stdout)) && isatty(fileno(stderr));
#else // !MOON_PRINT_USE_ANSI_COLOR_CODES
    return false;
#endif // MOON_PRINT_USE_ANSI_COLOR_CODES
}

// ANSI color codes:
inline const char * restore() noexcept { return colorPrintEnabled() ? "\033[0;1m"  : ""; }
inline const char * red()     noexcept { return colorPrintEnabled() ? "\033[31;1m" : ""; }
inline const char * green()   noexcept { return colorPrintEnabled() ? "\033[32;1m" : ""; }
inline const char * yellow()  noexcept { return colorPrintEnabled() ? "\033[33;1m" : ""; }
inline const char * blue()    noexcept { return colorPrintEnabled() ? "\033[34;1m" : ""; }
inline const char * magenta() noexcept { return colorPrintEnabled() ? "\033[35;1m" : ""; }
inline const char * cyan()    noexcept { return colorPrintEnabled() ? "\033[36;1m" : ""; }
inline const char * white()   noexcept { return colorPrintEnabled() ? "\033[37;1m" : ""; }

} // namespace color {}

// ========================================================
// Compiler/runtime output and error reporting:
// ========================================================

// Use this for compiler/parser error output only.
// Justs logs to stderr (std::cerr). Doesn't throw.
void compilerError(const std::string & errorMessage, const std::string & srcFile,
                   int srcLineNum, const std::string * offendingCode = nullptr);

// Use this for compiler/parser warning messages.
// Justs logs to stderr (std::cerr). Doesn't throw.
void compilerWarning(const std::string & warnMessage, const std::string & srcFile,
                     int srcLineNum, const std::string * offendingCode = nullptr);

// Use this to report fatal runtime/VM errors on the C++ side.
// Logs to stderr and throws RuntimeException.
[[noreturn]] void runtimeError(const std::string & errorMessage,
                               const std::string & srcFile,
                               int srcLineNum);

// Use this to report assertion failures on the C++ side.
// Logs to stderr and calls std::abort().
[[noreturn]] void runtimeAssertionError(const std::string & errorMessage,
                                        const std::string & srcFile,
                                        int srcLineNum);

// ========================================================
// Helper macros:
// ========================================================

// Convenience helper that adds the source file info:
#define MOON_RUNTIME_EXCEPTION(error) ::moon::runtimeError((error), __FILE__, __LINE__)

// Replacement for standard assert():
#if MOON_ENABLE_ASSERT
    #define MOON_ASSERT(expr)                                             \
        do                                                                \
        {                                                                 \
            if (!(expr))                                                  \
            {                                                             \
                ::moon::runtimeAssertionError(#expr, __FILE__, __LINE__); \
            }                                                             \
        } while (0)
#else // !MOON_ENABLE_ASSERT
    #define MOON_ASSERT(expr) /* no-op */
#endif // MOON_ENABLE_ASSERT

} // namespace moon {}

#endif // MOON_OUTPUT_HPP
