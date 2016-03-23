
// ================================================================================================
// -*- C++ -*-
// File: common.cpp
// Author: Guilherme R. Lampert
// Created on: 09/03/16
// Brief: Common helper code used by the compiler and runtime.
// ================================================================================================

#include "common.hpp"
#include "pool.hpp"
#include <cstdarg>

namespace moon
{

// ========================================================
// BaseException class:
// ========================================================

BaseException::BaseException(const char * message)
{
    if (message == nullptr)
    {
        message = "Unknown error";
    }
    std::snprintf(messageBuffer, arrayLength(messageBuffer), "Moon exception -> %s", message);
    messageBuffer[arrayLength(messageBuffer) - 1] = '\0'; // Ensure null terminated.
}

BaseException::BaseException(const std::string & message)
    : BaseException(message.c_str())
{
}

const char * BaseException::what() const noexcept
{
    return messageBuffer;
}

BaseException::~BaseException()
{
    // Anchors the virtual table to this file.
    // Do not remove.
}

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

std::string unescapeString(const char * escaped)
{
    MOON_ASSERT(escaped != nullptr);

    std::string unescaped;
    for (const char * ptr = escaped; *ptr != '\0'; ++ptr)
    {
        switch (*ptr)
        {
        case '\\' :
            unescaped.push_back('\\');
            unescaped.push_back('\\');
            break;
        case '\'' :
            unescaped.push_back('\'');
            break;
        case '\"' :
            unescaped.push_back('"');
            break;
        case '\n' :
            unescaped.push_back('\\');
            unescaped.push_back('n');
            break;
        case '\r' :
            unescaped.push_back('\\');
            unescaped.push_back('r');
            break;
        case '\t' :
            unescaped.push_back('\\');
            unescaped.push_back('t');
            break;
        default :
            unescaped.push_back(*ptr);
            break;
        } // switch (*ptr)
    }
    return unescaped;
}

std::string escapeString(const char * unescaped)
{
    MOON_ASSERT(unescaped != nullptr);

    std::string escaped;
    for (const char * ptr = unescaped; *ptr != '\0'; ++ptr)
    {
        const char c = *ptr;
        if (c == '\\')
        {
            const char next = *(ptr + 1); // Peek the next one
            switch (next)
            {
            case '0' :
                escaped.push_back('\0');
                ++ptr; break;
            case '\\' :
                escaped.push_back('\\');
                ++ptr; break;
            case '\'' :
                escaped.push_back('\'');
                ++ptr; break;
            case '"' :
                escaped.push_back('"');
                ++ptr; break;
            case 'n' :
                escaped.push_back('\n');
                ++ptr; break;
            case 'r' :
                escaped.push_back('\r');
                ++ptr; break;
            case 't' :
                escaped.push_back('\t');
                ++ptr; break;
            default :
                escaped.push_back(c);
                break;
            } // switch (next)
        }
        else
        {
            escaped.push_back(c);
        }
    }
    return escaped;
}

std::uint32_t hashCString(const char * cstr) noexcept
{
    MOON_ASSERT(cstr != nullptr);

    // Simple and fast One-at-a-Time (OAT) hash algorithm:
    //  http://en.wikipedia.org/wiki/Jenkins_hash_function
    //
    std::uint32_t h = 0;
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

std::string strPrintF(const char * format, ...)
{
// Suppress "format string is not a string literal" on GCC and Clang.
#ifdef __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wformat-nonliteral"
#endif // __GNUC__

    va_list vaArgs;
    char buffer[2048];
    const int available = arrayLength(buffer);

    va_start(vaArgs, format);
    int result = std::vsnprintf(buffer, available, format, vaArgs);
    va_end(vaArgs);

    if (result < 0)
    {
        result = 0;
    }
    else if (result >= available)
    {
        result = available - 1;
    }

    buffer[result] = '\0';
    return buffer;

#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif // __GNUC__
}

// ========================================================
// RcString => Reference counted string types:
// ========================================================

//
// ConstRcString:
//
ConstRcString * newConstRcString(const char * cstr)
{
    MOON_ASSERT(cstr != nullptr);
    const std::uint32_t strLen = static_cast<std::uint32_t>(std::strlen(cstr));

    const auto strHash    = hashCString(cstr);
    const auto memBytes   = sizeof(ConstRcString) + strLen + 1;
    std::uint8_t * memPtr = static_cast<std::uint8_t *>(::operator new(memBytes));

    auto rstr = reinterpret_cast<ConstRcString *>(memPtr);
    memPtr += sizeof(ConstRcString);

    char * strClone = reinterpret_cast<char *>(memPtr);
    std::memcpy(strClone, cstr, strLen);
    strClone[strLen] = '\0';

    return construct(rstr, { strClone, strLen, strHash, 1 });
}

void addRcStringRef(ConstRcString * rstr)
{
    MOON_ASSERT(rstr != nullptr);
    rstr->refCount++;
}

void releaseRcString(ConstRcString * rstr)
{
    MOON_ASSERT(rstr != nullptr);
    rstr->refCount--;
    if (rstr->refCount == 0)
    {
        ::operator delete(reinterpret_cast<void *>(rstr));
    }
}

//
// MutableRcString:
//
MutableRcString * newMutableRcString(const char * cstr)
{
    MOON_ASSERT(cstr != nullptr);
    const std::uint32_t strLen = static_cast<std::uint32_t>(std::strlen(cstr));

    const auto memBytes = sizeof(MutableRcString) + strLen + 1;
    std::uint8_t * memPtr = static_cast<std::uint8_t *>(::operator new(memBytes));

    auto rstr = reinterpret_cast<MutableRcString *>(memPtr);
    memPtr += sizeof(MutableRcString);

    char * strClone = reinterpret_cast<char *>(memPtr);
    std::memcpy(strClone, cstr, strLen);
    strClone[strLen] = '\0';

    return construct(rstr, { strClone, strLen, 1 });
}

void addRcStringRef(MutableRcString * rstr)
{
    MOON_ASSERT(rstr != nullptr);
    rstr->refCount++;
}

void releaseRcString(MutableRcString * rstr)
{
    MOON_ASSERT(rstr != nullptr);
    rstr->refCount--;
    if (rstr->refCount == 0)
    {
        ::operator delete(reinterpret_cast<void *>(rstr));
    }
}

} // namespace moon {}
