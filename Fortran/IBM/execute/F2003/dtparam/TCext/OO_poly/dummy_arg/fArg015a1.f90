! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg015a1.f
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
!*  DATE                       : 05/25/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : argument association (explicit-shape array and
!                               default initializations for INTENT(OUT)
!                               dummy-arg)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (8)
        integer, kind :: k1
        integer(k1)   :: id = -1

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child(k2,n1)    ! (8,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name = 'default'

        contains

        procedure :: print
    end type

    contains

    subroutine printBase(b)
        class(base(8)), intent(in) :: b

        print *, b%id
    end subroutine


    subroutine print (b)
        class(child(8,1,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine resetVal (b)
        class(base(8)), intent(out) :: b(3)
    end subroutine
end module


program fArg015a1
use m
    class (base(8)), allocatable, target :: b1(:,:)
    type (base(8)), pointer :: b2(:,:), b3(:)

    allocate (b1(3, 5), source=child(8,1,20)(1, 'test'))

    b2 => b1
    b3 => b1(2,::2)

    call resetVal (b2(1,:))
    call resetVal (b3)

    do j = 1, 5
        do i = 1, 3
            call b1(i, j)%print
        end do
    end do
end
