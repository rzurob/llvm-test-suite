!******************************************************************************
!*  ===========================================================================
!*  XL Fortran Test Case                                  IBM INTERNAL USE ONLY
!*  ===========================================================================
!*
!*  TEST CASE NAME  : F2008/value/pure/unit_tests/purevalue06.f
!*  TEST CASE TITLE : F2008: VALUE attr allowed for dummy args of PURE proc
!*  PROGRAMMER      : Gaby Baghdadi
!*  DATE            : 2010-12-01
!*  ORIGIN          : XL Fortran Compiler Development, IBM Torolab
!*  DRIVER STANZA   : xlf2003
!*
!*  DESCRIPTION
!*  - dummy data objects with value attribute before and after a procedure 
!*    dummy argument
!*  - the dummy procedure is also a pure function that takes a dummy argument 
!*    with value attribute, also modifies its arg
!*
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module m
contains
    integer pure function sqfunc (x)
        integer, value :: x
        sqfunc = x * x
        x = x + 100
    end function

    integer pure function sqrtfunc (x)
        integer, value :: x
        sqrtfunc = x ** 0.5
        x = x * 100
    end function

    integer pure function foosum (x,valfunc,y)
        integer, value :: x, y
        interface
            integer pure function valfunc (x)
                integer, value :: x
            end function
        end interface
        integer :: vx, vy
        integer :: ox, oy
        ox = x
        oy = y
        vx = valfunc(x)
        vy = valfunc(y)
        if (x /= ox .or. y/= oy) then
            foosum = -1
        else
            foosum = vx + vy
        endif
    end function
end module

use m
integer :: a, b, c
a = 2
b = 3
c = foosum(a,sqfunc,b)
if (any([a,b] /= [2,3])) stop 1
if (c /= 13) stop 2

a = 49
b = 81
c = foosum(a,sqrtfunc,b)
if (any([a,b] /= [49,81])) stop 3
if (c /= 16) stop 4
end
