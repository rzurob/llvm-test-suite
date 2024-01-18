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
! %GROUP: fArg010a5.f
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
!*  DATE                       : 05/13/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (VALUE attribute; non-poly
!                               dummy-arg associated with various actual arg)
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
    end type

    type, extends (base) :: child
        character*20 :: name
    end type

    type (child), protected :: c1 = child (20, 'c1')
end module

program fArg010a5
use m
    class (base), pointer :: b1
    type (child), parameter :: c_const = child (10, 'c_const')

    class (child), allocatable :: c2 (:)

    allocate (b1, source=child(1, 'b1_pointer'))

    allocate (c2(3:5))


    call abc (b1, b1%id)

    if (b1%id /= 1) error stop 1_4

    call abc (c_const%base, c_const%id)

    if (c_const%id /= 10) error stop 2_4

    call abc (c1%base, c1%id)

    if (c1%id /= 20) error stop 3_4

    c2%id = (/30, 40, 50/)
    c2%name = 'c2'

    do i = 3, 5
        call abc (c2(i)%base, i*10)
    end do

    deallocate (b1)

    contains

    subroutine abc (b, i)
        type (base), value :: b
        integer*4, value :: i

        if (b%id /= i) error stop 100_4

        b%id = b%id + 100
        i = b%id
    end subroutine
end
