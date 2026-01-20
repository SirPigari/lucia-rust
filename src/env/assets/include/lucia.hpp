#ifndef LUCIA_HPP
#define LUCIA_HPP

// .hpp header for C++ users to use lucia.h

extern "C" {
    #include "lucia.h"
}

#ifdef LUCIA_CHECK_SIZES
extern "C" {
    #include "lucia_size_check.h"
}
#endif

#endif // LUCIA_HPP