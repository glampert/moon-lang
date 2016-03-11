
// ================================================================================================
// -*- C++ -*-
// File: common.cpp
// Author: Guilherme R. Lampert
// Created on: 09/03/16
// Brief: Common helper code used by the compiler and runtime.
// ================================================================================================

#include "common.hpp"

namespace moon
{

// ========================================================
// Miscellaneous utilities:
// ========================================================

std::string trimTrailingFloatZeros(std::string trimmed)
{
    // Only proceed if the number is decimal (has a dot somewhere):
    if (trimmed.find_last_of('.') == std::string::npos)
    {
        return trimmed;
    }

    // Remove trailing zeros:
    while (!trimmed.empty() && trimmed.back() == '0')
    {
        trimmed.pop_back();
    }

    // If the dot was left alone at the end, remove it too:
    if (!trimmed.empty() && trimmed.back() == '.')
    {
        trimmed.pop_back();
    }
    return trimmed;
}

std::size_t hashCString(const char * cstr) noexcept
{
    MOON_ASSERT(cstr != nullptr);

    // Simple and fast One-at-a-Time (OAT) hash algorithm:
    //  http://en.wikipedia.org/wiki/Jenkins_hash_function
    //
    std::size_t h = 0;
    while (*cstr != '\0')
    {
        h += *cstr++;
        h += (h << 10);
        h ^= (h >>  6);
    }
    h += (h <<  3);
    h ^= (h >> 11);
    h += (h << 15);

    return h;
}

const char * getEmptyCString() noexcept
{
    static const char emptyStr[]{ '\0', '\0', '\0', '\0' };
    return emptyStr;
}

} // namespace moon {}
