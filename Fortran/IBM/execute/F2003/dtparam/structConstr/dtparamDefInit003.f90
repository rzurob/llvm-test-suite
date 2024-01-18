!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 02/28/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constr)
!                               Case: Structure constructor for derived type
!                               with default initializations; component is
!                               character type.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (len, asciiVal)
        integer, kind :: len
        integer(1), kind :: asciiVal

        character(len) :: name = repeat(achar(asciiVal), len)
    end type

    type(base(10, 66_1)) :: b1 = base(10, 66_1)()
end module

module n
use m
    type speller (len, asciiVal)
        integer, kind :: len
        integer(1), kind :: asciiVal

        type(base(len, asciiVal)) :: name = base (len, asciiVal)()
        type(base(3, 65)) :: ref = base(3,65)(name='ref')
    end type

    type (speller(30, 77)) :: s1 = speller(30, 77)()
    type (speller(1, 88)) :: x = speller(1, 88)()
    type (speller(1, 76)) :: l = speller(1, 76)()
    type (speller(1, 70)) :: f = speller(1, 70)()
end module

program dtparamDefInit003
use n
    print *, b1
    print *, s1
    print *, x,l,f
end
