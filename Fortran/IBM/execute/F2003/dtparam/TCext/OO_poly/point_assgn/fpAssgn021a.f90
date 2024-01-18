! GB DTP extension using:
! ftcx_dtp -qck -qnol -qnodeferredlp /tstdev/OO_poly/point_assgn/fpAssgn021a.f
! opt variations: -qnock -ql -qdeferredlp

! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn021a.f
! %VERIFY: fpAssgn021a.out:fpAssgn021a.vf
! %STDIN:
! %STDOUT: fpAssgn021a.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/15/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (scalar pointer
!*                               assigned to array elements; components and
!*                               bindings; array is poly array)
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
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      id

        contains

        procedure, non_overridable :: getID => baseID
        procedure, non_overridable :: assgnID => assgnBaseID
        procedure :: print => basePrint
    end type

    class (base(4)), allocatable, target :: b1_m (:)

    contains

    integer*4 function baseID (b)
        class (base(4)), intent(in) :: b

        baseID = b%id
    end function

    subroutine assgnBaseID (b, i)
        class (base(4)), intent(inout) :: b
        integer*4, intent(in) :: i

        b%id = i
    end subroutine

    subroutine basePrint (b)
        class (base(4)), intent(in) :: b

        print *, b%id
    end subroutine
end module


module m1
use m, only : base

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printChild (b)
        class (child(4,1,*)), intent(in) :: b

        print *, b%getID(), b%name
    end subroutine
end module

program fpAssgn021
use m
use m1
    class (base(4)), pointer :: b_ptr
    class (child(4,1,20)), allocatable, target :: c_allo(:)

    integer*4 :: i

    allocate (c_allo(10))

    do i = 1, size(c_allo)
        b_ptr => c_allo(i)
        c_allo%name = 'c_allo'

        call b_ptr%assgnID (i)
    end do

    do i = 1, size(c_allo)
        b_ptr => c_allo(i)

        if (b_ptr%id /= b_ptr%getID()) error stop 1_4
        if (b_ptr%getID() /= i) error stop 2_4

        call b_ptr%print
    end do

    deallocate (c_allo)
end
