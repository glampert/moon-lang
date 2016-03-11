
// ================================================================================================
// -*- C++ -*-
// File: output.cpp
// Author: Guilherme R. Lampert
// Created on: 04/03/16
// Brief: Runtime output printing helpers.
// ================================================================================================

#include "output.hpp"
#include "common.hpp"

namespace moon
{

void compilerError(const std::string & errorMessage, const std::string & srcFile,
                   const int srcLineNum, const std::string * offendingCode)
{
    const std::string errorTag = color::red() + toString("error:") + color::restore();
    std::string message = srcFile + "(" + toString(srcLineNum) + "): " + errorTag + " " + errorMessage;

    if (offendingCode != nullptr && !offendingCode->empty())
    {
        message += "\n";
        message += color::cyan();
        message += *offendingCode;
        if (message.back() == '\n') { message.pop_back(); } // No newlines before the '<---'
        message += color::restore();
        message += " <---\n";
    }

    std::cerr << message << "\n";
}

void compilerWarning(const std::string & warnMessage, const std::string & srcFile,
                     const int srcLineNum, const std::string * offendingCode)
{
    const std::string warningTag = color::magenta() + toString("warning:") + color::restore();
    std::string message = srcFile + "(" + toString(srcLineNum) + "): " + warningTag + " " + warnMessage;

    if (offendingCode != nullptr && !offendingCode->empty())
    {
        message += "\n";
        message += color::cyan();
        message += *offendingCode;
        if (message.back() == '\n') { message.pop_back(); } // No newlines before the '<---'
        message += color::restore();
        message += " <---\n";
    }

    std::cerr << message << "\n";
}

[[noreturn]]
void runtimeError(const std::string & errorMessage, const std::string & srcFile, const int srcLineNum)
{
    const std::string errorTag = color::red() + toString("runtime error:") + color::restore();
    const std::string message  = srcFile + "(" + toString(srcLineNum) + "): " + errorTag + " " + errorMessage;

    std::cerr << message << std::endl;
    throw RuntimeException{ "Runtime error" };
}

[[noreturn]]
void runtimeAssertionError(const std::string & errorMessage, const std::string & srcFile, const int srcLineNum)
{
    const std::string errorTag = color::yellow() + toString("runtime assertion failed:") + color::restore();
    const std::string message  = srcFile + "(" + toString(srcLineNum) + "): " + errorTag + " " + errorMessage;

    std::cerr << message << std::endl;
    std::abort();
}

} // namespace moon {}
