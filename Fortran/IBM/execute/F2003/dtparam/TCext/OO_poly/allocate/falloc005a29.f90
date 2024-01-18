! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc005a29.f
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
!*  DATE                       : 05/05/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : allocate statement (named constants used in
!                               the source-expr; use array sections of named
!                               constants of derived type)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (8)
        integer, kind :: k1
        real(k1)         r1

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child(k2,n1)    ! (8,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class(base(8)), intent(in) :: b

        write (*, '(f12.3)') b%r1
    end subroutine

    subroutine printChild (b)
        class (child(8,1,*)), intent(in) :: b

        write (*, '(f12.3,tr1,a)') b%r1, b%name
    end subroutine
end module

program falloc005a29
use m
    type (child(8,1,20)), parameter :: c_const (10) = &
            (/(child(8,1,20)(i*1.1_8, 'xlftest'), i=10,1, -1)/)

    class(base(8)), allocatable :: b1, b2(:)

    class (base(8)), pointer :: b3(:)


    allocate (b1, source=c_const(3))

    allocate (b2(5), source=c_const(:5)%base)

    allocate (b3(2), source=c_const(7:10:3))

    !! verify results
    call b1%print

    do i = 1, 5
        call b2(i)%print
    end do

    call b3(1)%print
    call b3(2)%print
end
