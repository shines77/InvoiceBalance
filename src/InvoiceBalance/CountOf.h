
#ifndef GCC_COUNT_OF_H
#define GCC_COUNT_OF_H

#if !defined(_MSC_VER)

#if defined(_M_X64) || defined(_M_AMD64) || defined(_M_IA64)
  #define _UNALIGNED __unaligned
#else
  #define _UNALIGNED
#endif // _M_X64, _M_ARM

/* _countof helper */
#if !defined(_countof)
  #if !defined(__cplusplus)
    #define _countof(_Array) (sizeof(_Array) / sizeof(_Array[0]))
  #else // !defined (__cplusplus)
    extern "C++"
    {
        template <typename _CountofType, size_t _SizeOfArray>
        char (* __countof_helper(_UNALIGNED _CountofType(&_Array)[_SizeOfArray]))[_SizeOfArray];

        #define _countof(_Array) (sizeof(*__countof_helper(_Array)) + 0)
    }
  #endif // !defined(__cplusplus)
#endif // !defined(_countof)

#endif // !_MSC_VER

#endif // GCC_COUNT_OF_H
