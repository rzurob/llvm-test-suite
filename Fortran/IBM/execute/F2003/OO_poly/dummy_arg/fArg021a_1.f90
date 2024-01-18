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
! %GROUP: fArg021a_1.f
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
!*  DATE                       : 06/01/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (named-const used as the
!                               actual-arg for unlimited poly dummy-arg; also
!                               tests that compiler will use temporaries that
!                               duplicate in value for the calls)
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

    private abc
    contains

    subroutine test1 (y)
        class (*) :: y

        call abc (y)
    end subroutine

    subroutine abc (x)
        class(*), intent(inout) :: x
    end subroutine
end module

program fArg021a
use m
    integer*1, parameter :: i0 = 0
    integer*2, parameter :: i1 = 1
    integer*4, parameter :: i2 = 2
    integer*8 :: i3
    parameter (i3 = 3)

    real*4 r1
    real*8 r2
    real*16 r3

    parameter (r1 = 1.0, r2 = 2.0d0, r3 = 3.0q0)

    logical*1, parameter :: l1 = ('abc' > 'ABC')
    logical*2 l2
    logical*4 l3

    parameter (l2 = l1, l3 = .false.)

    character c1
    character*20, parameter :: c2 = 'xlf test for OO features'
    parameter (c1 = 'f')

    complex*8, parameter :: cmplx1 = (0.0, 0.0)
    complex*16, parameter :: cmplx2 = (1.0d0, 1.0d0)
    complex*32, parameter :: cmplx3 = (2.0q0, 2.0q0)

    call test1 (i0)
    call test1 (i1)
    call test1 (i2)
    call test1 (i3)

    call test1 (r1)
    call test1 (r2)
    call test1 (r3)

    call test1 (l1)
    call test1 (l2)
    call test1 (l3)

    call test1 (c1)
    call test1 (c2)
    call test1 (c2(2:10))

    call test1 (cmplx1)
    call test1 (cmplx2)
    call test1 (cmplx3)
end
