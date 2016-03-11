
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
// class ObjectPool<T, Granularity>
// ========================================================

//
// Pool of fixed-size memory blocks (similar to a list of arrays).
//
// This pool allocator operates as a linked list of small arrays.
// Each array is a pool of blocks with the size of 'T' template parameter.
// Template parameter `Granularity` defines the size in objects of type 'T'
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
    std::size_t Granularity
>
class ObjectPool final
{
public:

     ObjectPool(); // Empty pool; no allocation until first use.
    ~ObjectPool(); // Drains the pool.

    // Not copyable.
    ObjectPool(const ObjectPool &) = delete;
    ObjectPool & operator = (const ObjectPool &) = delete;

    // Allocates a single memory block of size 'T' and
    // returns an uninitialized pointer to it.
    T * allocate();

    // Deallocates a memory block previously allocated by `allocate()`.
    // Pointer may be null, in which case this is a no-op. NOTE: Class destructor NOT called!
    void deallocate(void * ptr);

    // Frees all blocks, reseting the pool allocator to its initial state.
    // WARNING: Calling this method will invalidate any memory block still
    // alive that was previously allocated from this pool.
    void drain();

    // Miscellaneous stats queries:
    std::size_t getTotalAllocs()  const noexcept;
    std::size_t getTotalFrees()   const noexcept;
    std::size_t getObjectsAlive() const noexcept;
    std::size_t getGranularity()  const noexcept;
    std::size_t getSize()         const noexcept;

private:

    union PoolObj
    {
        alignas(T) std::uint8_t userData[sizeof(T)];
        PoolObj * next;
    };

    struct PoolBlock
    {
        PoolObj objects[Granularity];
        PoolBlock * next;
    };

    PoolBlock * blockList;      // List of all blocks/pools.
    PoolObj   * freeList;       // List of free objects that can be recycled.
    std::size_t allocCount;     // Total calls to `allocate()`.
    std::size_t objectCount;    // User objects ('T' instances) currently active.
    std::size_t poolBlockCount; // Size in blocks of the `blockList`.
};

// ========================================================
// ObjectPool inline implementation:
// ========================================================

template<typename T, std::size_t Granularity>
ObjectPool<T, Granularity>::ObjectPool()
    : blockList      { nullptr }
    , freeList       { nullptr }
    , allocCount     { 0 }
    , objectCount    { 0 }
    , poolBlockCount { 0 }
{
    // Allocates memory when the first object is requested.
}

template<typename T, std::size_t Granularity>
ObjectPool<T, Granularity>::~ObjectPool()
{
    drain();
}

template<typename T, std::size_t Granularity>
T * ObjectPool<T, Granularity>::allocate()
{
    if (freeList == nullptr)
    {
        PoolBlock * newBlock = new PoolBlock();
        newBlock->next = blockList;
        blockList = newBlock;

        ++poolBlockCount;

        // All objects in the new pool block are appended
        // to the free list, since they are ready to be used.
        for (std::size_t i = 0; i < Granularity; ++i)
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
    #if MOON_DEBUG_MEMORY
    std::memset(object, 0xAA, sizeof(PoolObj));
    #endif // MOON_DEBUG_MEMORY

    return reinterpret_cast<T *>(object);
}

template<typename T, std::size_t Granularity>
void ObjectPool<T, Granularity>::deallocate(void * ptr)
{
    MOON_ASSERT(objectCount != 0);
    if (ptr == nullptr)
    {
        return;
    }

    // Fill user portion with a known pattern to help
    // detecting post deallocation usage attempts.
    #if MOON_DEBUG_MEMORY
    std::memset(ptr, 0xFE, sizeof(PoolObj));
    #endif // MOON_DEBUG_MEMORY

    // Add back to free list's head. Memory not actually freed now.
    PoolObj * object = reinterpret_cast<PoolObj *>(ptr);
    object->next = freeList;
    freeList = object;

    --objectCount;
}

template<typename T, std::size_t Granularity>
void ObjectPool<T, Granularity>::drain()
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

template<typename T, std::size_t Granularity>
std::size_t ObjectPool<T, Granularity>::getTotalAllocs() const noexcept
{
    return allocCount;
}

template<typename T, std::size_t Granularity>
std::size_t ObjectPool<T, Granularity>::getTotalFrees() const noexcept
{
    return allocCount - objectCount;
}

template<typename T, std::size_t Granularity>
std::size_t ObjectPool<T, Granularity>::getObjectsAlive() const noexcept
{
    return objectCount;
}

template<typename T, std::size_t Granularity>
std::size_t ObjectPool<T, Granularity>::getGranularity() const noexcept
{
    return Granularity;
}

template<typename T, std::size_t Granularity>
std::size_t ObjectPool<T, Granularity>::getSize() const noexcept
{
    return poolBlockCount;
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
