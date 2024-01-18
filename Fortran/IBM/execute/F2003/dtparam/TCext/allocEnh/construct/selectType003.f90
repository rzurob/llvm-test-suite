! GB DTP extension using:
! ftcx_dtp -ql -qnodefaultpv -qdeferredlp -qreuse=base /tstdev/F2003/allocEnh/construct/selectType003.f
! opt variations: -qnol -qdefaultpv -qnodeferredlp -qreuse=none

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
!*  DATE                       : 09/20/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test that associate name's allocatable component
!                               appear as the variable in an intrinsic
!                               assignment.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n1,k1)    ! (20,4)
        integer, kind         :: k1
        integer, len          :: n1
        real(k1), allocatable :: data(:)

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child    ! (20,4)
        type(base(:,k1)), allocatable :: r1

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class(base(*,4)), intent(in) :: b

        if (allocated(b%data)) then
            print *, lbound(b%data), 'to', ubound(b%data)

            write (*, '(12g10.2)') b%data
        end if
    end subroutine

    subroutine printChild (b)
        class(child(*,4)), intent(in) :: b

        call b%base%print

        if (allocated(b%r1)) then
            print *, 'child type component r1:'

            call b%r1%print
        end if
    end subroutine
end module

program selectType003
use m
    class(base(:,4)), allocatable :: b1

    allocate(child(20,4) :: b1)

    select type (x => b1)
        type is (child(*,4))
            x%data = (/(j, j=1,100)/)
            x%r1 = base(20,4)(x%data)

        class default
            stop 10
    end select

    call b1%print
end
