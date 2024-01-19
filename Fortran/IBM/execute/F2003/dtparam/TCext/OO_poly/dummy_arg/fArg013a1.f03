! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg013a1.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/13/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (TARGET attributes with
!                               both dummy-arg and actual-arg; still scalars)
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
        integer(k1)   :: id = -1

        contains

        procedure :: print => printBase
    end type

    type, extends (base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name = 'default'

        contains

        procedure :: print => printChild
    end type

    class (base(4)), pointer :: b_ptr, b_ptr2

    contains

    subroutine printChild (b)
        class (child(4,1,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine createb1b2 (b1, b2)
        class (base(4)), pointer, intent(out) :: b1
        class (base(4)), target, intent(out), allocatable :: b2

        if (allocated (b2)) error stop 100_4

        allocate (b2)

        b1 => b2
        b_ptr2 => b2
    end subroutine

    subroutine createb1b2_v (b1, b2)
        class (base(4)), pointer, intent(out) :: b1
        class (base(4)), target, intent(out) :: b2

        b2%id = 10

        b1 => b2
    end subroutine
end module

program fArg013a1
use m
    class (base(4)), allocatable, target :: b1

    allocate (b1, source=child(4,1,20) (1, 'b1'))

    call createb1b2 (b_ptr, b1)

    if ((.not. associated (b_ptr, b1)) .or. (.not. associated (b_ptr, b_ptr2)))&
        error stop 1_4

    call b1%print

    deallocate (b1)

    allocate (b1, source=child(4,1,20) (1, 'b1'))

    b_ptr => b1

    call createb1b2_v (b_ptr2, b1)

    call b1%print

    if (.not. associated (b_ptr2, b1)) error stop 2_4

    if (.not. associated (b_ptr, b_ptr2)) error stop 3_4

    call b_ptr%print
end
