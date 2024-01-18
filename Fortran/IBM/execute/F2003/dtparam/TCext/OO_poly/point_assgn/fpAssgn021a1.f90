! GB DTP extension using:
! ftcx_dtp -qck -ql -qnodeferredlp -qreuse=base /tstdev/OO_poly/point_assgn/fpAssgn021a1.f
! opt variations: -qnock -qnol -qdeferredlp -qreuse=none

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
! %GROUP: fpAssgn021a1.f
! %VERIFY: fpAssgn021a1.out:fpAssgn021a1.vf
! %STDIN:
! %STDOUT: fpAssgn021a1.out
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
!*  DATE                       : 03/15/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : data pointer assignment (scalar pointer
!*                               assigned to array elements; components and
!*                               bindings; array is two dimensional poly-array)
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
    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      id

        contains

        procedure, non_overridable :: getID => baseID
        procedure, non_overridable :: assgnID => assgnBaseID
        procedure :: print => basePrint
    end type

    class (base(20,4)), allocatable, target :: b1_m (:)

    contains

    integer*4 function baseID (b)
        class (base(*,4)), intent(in) :: b

        baseID = b%id
    end function

    subroutine assgnBaseID (b, i)
        class (base(*,4)), intent(inout) :: b
        integer*4, intent(in) :: i

        b%id = i
    end subroutine

    subroutine basePrint (b)
        class (base(*,4)), intent(in) :: b

        print *, b%id
    end subroutine
end module


module m1
use m, only : base

    type, extends(base) :: child(k2)    ! (20,4,1)
        integer, kind             :: k2
        character(kind=k2,len=n1) :: name

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printChild (b)
        class (child(*,4,1)), intent(in) :: b

        print *, b%getID(), b%name
    end subroutine

    character(2) function int2char (i)
        integer*4, intent(in) :: i

        write (int2char, '(i2.2)') i
    end function
end module

program fpAssgn021
use m
use m1
    class (base(20,4)), pointer :: b_ptr

    class (child(20,4,1)), pointer :: c1 (:,:)
    integer*4 :: i, j

    allocate (c1 (10, 20))

    do i = 1, 10
        do j = 1, 20
            b_ptr => c1(i, j)
            c1(i, j)%name = 'c1.'//int2char(i)//'.'//int2char(j)

            call b_ptr%assgnID (i+j)
        end do
    end do

    do i = 1, 10
        do j = 1, 20
            b_ptr => c1(i, j)

            if (b_ptr%id /= b_ptr%getID()) error stop 1_4
            if (b_ptr%id /= i+j) error stop 2_4

            call b_ptr%print
        end do
    end do

    deallocate (c1)
end
