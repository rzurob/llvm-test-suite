! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg017.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/26/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (dummy-arg with TARGET
!                               attribute associated with actual-arg with no
!                               TARGET attribute)
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
        integer(k1)   :: id

        contains

        procedure :: print => printBase
    end type

    type, extends (base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name

        contains

        procedure :: print => printChild
    end type

    class (base(4)), allocatable :: b1_m
    class (base(4)), pointer :: b_ptr, b_ptr2 (:)

    contains

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(4,1,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module


program fArg017
use m
    interface
        subroutine test1 (b)
        use m
            class (base(4)), target, intent(inout) :: b
        end subroutine

        subroutine test2 (b)
        use m
            class (base(4)), target, intent(inout) :: b(:)
        end subroutine
    end interface

    type (child(4,1,20)) :: c1(2:4)

    allocate (b1_m, source=child(4,1,20)(1, 'b1_m'))

    call test1 (b1_m)

    c1%name = (/'c1_2', 'c1_3','c1_4'/)

    call test2 (c1)

    do i = 2, 4
        call c1(i)%print
    end do
end

subroutine test1 (b)
use m
    class (base(4)), target, intent(inout) :: b

    b_ptr => b

    call b_ptr%print

end subroutine

subroutine test2 (b)
use m
    class (base(4)), target, intent(inout) :: b(:)

    b_ptr2 => b

    b_ptr2%id = -10
end subroutine
