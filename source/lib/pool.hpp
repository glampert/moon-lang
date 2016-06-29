
// ================================================================================================
// -*- C++ -*-
// File: pool.hpp
// Author: Guilherme R. Lampert
// Created on: 14/06/15
// Brief: Custom generic pool allocator.
// ================================================================================================

#ifndef MOON_POOL_HPP
#define MOON_POOL_HPP

#include "common.hpp"
#include <memory>
#include <utility>

namespace moon
{

// ========================================================
// template class Pool<T, Granularity>:
// ========================================================

//
// Pool of fixed-size memory blocks (similar to a list of arrays).
//
// This pool allocator operates as a linked list of small arrays.
// Each array is a pool of blocks with the size of 'T' template parameter.
// Template parameter 'Granularity' defines the size in objects of type 'T'
// of such arrays.
//
// allocate() will return an uninitialized memory block.
// The user is responsible for calling construct() on it to run class
// constructors if necessary, and destroy() to call class destructor
// before deallocating the block.
//
template
<
    typename T,
    int Granularity
>
class Pool final
{
public:

     Pool(); // Empty pool; no allocation until first use.
    ~Pool(); // Drains the pool.

    // Not copyable.
    Pool(const Pool &) = delete;
    Pool & operator = (const Pool &) = delete;

    // Allocates a single memory block of size 'T' and
    // returns an uninitialized pointer to it.
    T * allocate();

    // Deallocates a memory block previously allocated by 'allocate()'.
    // Pointer may be null, in which case this is a no-op. NOTE: Class destructor NOT called!
    void deallocate(void * ptr);

    // Frees all blocks, reseting the pool allocator to its initial state.
    // WARNING: Calling this method will invalidate any memory block still
    // alive that was previously allocated from this pool.
    void drain();

    // Miscellaneous stats queries:
    int getTotalAllocs()  const noexcept;
    int getTotalFrees()   const noexcept;
    int getObjectsAlive() const noexcept;
    int getSize()         const noexcept;

    static std::size_t getGranularity() noexcept;
    static std::size_t getObjectSize()  noexcept;

private:

    // Fill patterns for debug allocations.
    #if MOON_DEBUG
    static constexpr int AllocFillVal = 0xAA;
    static constexpr int FreeFillVal  = 0xFE;
    #endif // MOON_DEBUG

    union PoolObj
    {
        alignas(T) UInt8 userData[sizeof(T)];
        PoolObj * next;
    };

    struct PoolBlock
    {
        PoolObj objects[Granularity];
        PoolBlock * next;
    };

    PoolBlock * blockList; // List of all blocks/pools.
    PoolObj   * freeList;  // List of free objects that can be recycled.
    int allocCount;        // Total calls to 'allocate()'.
    int objectCount;       // User objects ('T' instances) currently active.
    int poolBlockCount;    // Size in blocks of the 'blockList'.
};

// ========================================================
// Pool<T> inline implementation:
// ========================================================

template<typename T, int Granularity>
Pool<T, Granularity>::Pool()
    : blockList      { nullptr }
    , freeList       { nullptr }
    , allocCount     { 0 }
    , objectCount    { 0 }
    , poolBlockCount { 0 }
{
    // Allocates memory when the first object is requested.
}

template<typename T, int Granularity>
Pool<T, Granularity>::~Pool()
{
    drain();
}

template<typename T, int Granularity>
T * Pool<T, Granularity>::allocate()
{
    if (freeList == nullptr)
    {
        auto newBlock  = new PoolBlock{};
        newBlock->next = blockList;
        blockList      = newBlock;

        ++poolBlockCount;

        // All objects in the new pool block are appended
        // to the free list, since they are ready to be used.
        for (int i = 0; i < Granularity; ++i)
        {
            newBlock->objects[i].next = freeList;
            freeList = &newBlock->objects[i];
        }
    }

    ++allocCount;
    ++objectCount;

    // Fetch one from the free list's head:
    PoolObj * object = freeList;
    freeList = freeList->next;

    // Initializing the object with a known pattern
    // to help detecting memory errors.
    #if MOON_DEBUG
    std::memset(object, AllocFillVal, sizeof(PoolObj));
    #endif // MOON_DEBUG

    return reinterpret_cast<T *>(object);
}

template<typename T, int Granularity>
void Pool<T, Granularity>::deallocate(void * ptr)
{
    MOON_ASSERT(objectCount != 0);
    if (ptr == nullptr)
    {
        return;
    }

    // Fill user portion with a known pattern to help
    // detecting post-deallocation usage attempts.
    #if MOON_DEBUG
    std::memset(ptr, FreeFillVal, sizeof(PoolObj));
    #endif // MOON_DEBUG

    // Add back to free list's head. Memory not actually freed now.
    auto object  = static_cast<PoolObj *>(ptr);
    object->next = freeList;
    freeList     = object;

    --objectCount;
}

template<typename T, int Granularity>
void Pool<T, Granularity>::drain()
{
    while (blockList != nullptr)
    {
        PoolBlock * block = blockList;
        blockList = blockList->next;
        delete block;
    }

    blockList      = nullptr;
    freeList       = nullptr;
    allocCount     = 0;
    objectCount    = 0;
    poolBlockCount = 0;
}

template<typename T, int Granularity>
int Pool<T, Granularity>::getTotalAllocs() const noexcept
{
    return allocCount;
}

template<typename T, int Granularity>
int Pool<T, Granularity>::getTotalFrees() const noexcept
{
    return allocCount - objectCount;
}

template<typename T, int Granularity>
int Pool<T, Granularity>::getObjectsAlive() const noexcept
{
    return objectCount;
}

template<typename T, int Granularity>
int Pool<T, Granularity>::getSize() const noexcept
{
    return poolBlockCount;
}

template<typename T, int Granularity>
std::size_t Pool<T, Granularity>::getGranularity() noexcept
{
    return Granularity;
}

template<typename T, int Granularity>
std::size_t Pool<T, Granularity>::getObjectSize() noexcept
{
    return sizeof(T);
}

// ========================================================
// construct() / destroy() helpers:
// ========================================================

template<typename T>
T * construct(T * obj, const T & other) // Copy constructor
{
    return ::new(obj) T(other);
}

template<typename T, typename... Args>
T * construct(T * obj, Args&&... args) // Parameterized or default constructor
{
    return ::new(obj) T(std::forward<Args>(args)...);
}

template<typename T>
void destroy(T * obj) noexcept
{
    if (obj != nullptr)
    {
        obj->~T();
    }
}

} // namespace moon {}

#endif // MOON_POOL_HPP
