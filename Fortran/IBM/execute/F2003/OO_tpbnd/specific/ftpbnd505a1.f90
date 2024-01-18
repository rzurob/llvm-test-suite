!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ftpbnd505a1.f
! %VERIFY: 
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: 
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 03/16/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : specific type bound (elemental PASS subroutine
!*                               binding; basic assignment operation)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer*4 :: id

        contains

        procedure :: assgn => assgnBase
    end type

    contains

    elemental subroutine assgnBase (b, b1)
        class (base), intent(out) :: b
        class (base), intent(in) :: b1

        b%id = b1%id
    end subroutine

end module

program ftpbnd505a1
use m
    type (base) :: b1, b2, b3(10), b4(10)

    b2 = base (100)

    b4 = (/(base(i), i=2,11)/)

    !! assign value from scalar to scalar
    call b1%assgn(b2)

    if (b1%id /= 100) error stop 1_4


    !! assign values from array to array
    call b3%assgn(b4)

    if (any (b3%id /= b4%id)) error stop 2_4

    b3 = base(0)
    !! assign values from element to element
    do i = 1, 10
        call b3(i)%assgn(b4(i))
    end do

    do i = 1, 10
        if (b3(i)%id /= i+1) error stop 3_4
    end do


    !! assign value to an array from a scalar
    call b3%assgn(base(-1))

    if (any (b3%id /= -1)) error stop 4_4
end
