
// ================================================================================================
// -*- C++ -*-
// File: registry.hpp
// Author: Guilherme R. Lampert
// Created on: 03/04/16
// Brief: Helper base type with common operations for our many table-like classes.
// ================================================================================================

#ifndef MOON_REGISTRY_HPP
#define MOON_REGISTRY_HPP

#include "common.hpp"

namespace moon
{

// ========================================================
// template class Registry<T>:
// ========================================================

template<typename T>
class Registry
{
public:

    using StoredType     = T;
    using TableType      = HashTableConstRcStr<StoredType>;
    using TableMutIter   = typename TableType::iterator;
    using TableConstIter = typename TableType::const_iterator;

    Registry(const Registry &) = delete;
    Registry & operator = (const Registry &) = delete;

    bool isEmpty() const noexcept { return table.empty(); }
    int  getSize() const noexcept { return static_cast<int>(table.size()); }

    TableMutIter begin() noexcept { return table.begin(); }
    TableMutIter end()   noexcept { return table.end();   }

    TableConstIter begin() const noexcept { return table.cbegin(); }
    TableConstIter end()   const noexcept { return table.cend();   }

protected:

    // The registry of [key, value] pairs.
    TableType table;

    // Registry is not meant to be used as a standalone type.
    // Its purpose is to serve as a base class for table-like
    // helper containers used internally by the runtime and compiler.
    Registry() = default;

    // Release the ref counted keys:
    ~Registry()
    {
        for (auto && entry : table)
        {
            releaseRcString(entry.first);
        }
    }

    //
    // Add unique:
    //

    StoredType addInternal(ConstRcString * key, StoredType val)
    {
        MOON_ASSERT(isRcStringValid(key));

        const auto result = table.insert(std::make_pair(key, val));
        if (result.second == false)
        {
            MOON_RUNTIME_EXCEPTION("registry key collision for '" + toString(key) + "'");
        }

        addRcStringRef(key); // Reference the key now stored in the table.
        return result.first->second;
    }

    StoredType addInternal(const char * key, StoredType val)
    {
        // This one allocates a new ref string from the C-string.
        ConstRcStrUPtr rstr{ newConstRcString(key) };
        return addInternal(rstr.get(), val);
    }

    //
    // Lookup by key:
    //

    StoredType findInternal(ConstRcString * const key, StoredType notFoundValue = StoredType{}) const
    {
        MOON_ASSERT(isRcStringValid(key));

        auto && iter = table.find(key);
        if (iter != std::end(table))
        {
            return iter->second;
        }

        return notFoundValue;
    }

    StoredType findInternal(const char * key, StoredType notFoundValue = StoredType{}) const
    {
        // We can get away with a cheap temp because find()
        // will never add ref to the input string.
        ConstRcString tempRcStr{
        /* chars    = */ key,
        /* length   = */ static_cast<UInt32>(std::strlen(key)),
        /* hashVal  = */ hashCString(key),
        /* refCount = */ 1 };
        return findInternal(&tempRcStr, notFoundValue);
    }
};

} // namespace moon {}

#endif // MOON_REGISTRY_HPP
