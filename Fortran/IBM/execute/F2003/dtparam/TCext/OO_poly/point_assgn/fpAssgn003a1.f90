! GB DTP extension using:
! ftcx_dtp -qnol -qnodeferredlp /tstdev/OO_poly/point_assgn/fpAssgn003a1.f
! opt variations: -qck -ql -qdeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/19/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (non-poly pointer
!*                               assigned to poly-allocatable targets; tests
!*                               the dynamic types using bindings)
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
        integer, kind        :: k1
        integer(k1), private :: id = 0

        contains

        procedure, non_overridable :: getID => getBaseID
        procedure, non_overridable :: assgnID => assgnBaseID
        procedure :: print => printBase
    end type

    contains

    subroutine assgnBaseID (b, i)
        class (base(4)), intent(inout) :: b
        integer*4, intent(in) :: i

        b%id = i
    end subroutine

    integer*4 function getBaseID (b)
        class (base(4)), intent(in) :: b

        getBaseID = b%id
    end function

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        print *, b%id
    end subroutine
end module

module m1
use m, only : base
    class (base(4)), pointer :: b1_m, b2_m(:)

    type, extends(base) :: child(n1)    ! (4,20)
        integer, len  :: n1
        character(n1) :: name = ''

        contains

        procedure :: print => printChild
    end type

    class (child(4,20)), target, allocatable :: c1_m, c2_m(:)

    contains

    subroutine printChild (b)
        class (child(4,*)), intent(in) :: b

        print *, b%getID(), b%name
    end subroutine

    subroutine printData (b)
        class (base(4)), intent(in) :: b

        call b%print
    end subroutine

    character(2) function int2Char (i)
        integer, intent(in) :: i

        write (int2Char, '(i2.2)') i
    end function
end module

program fpAssgn003a1
use m1

    type (base(4)), pointer :: b1, b2(:)

    !! test c1_m, c2_m as targets

    allocate (c1_m, c2_m(20))

    b1_m => c1_m
    b2_m => c2_m

    call b1_m%assgnID(1)
    c1_m%name = 'b1_m'

    do i = 1, 20
        call b2_m(i)%assgnID(i+1)

        c2_m(i)%name = 'c2_m:'//int2Char (i)
    end do

    !! now do assignment to non-poly pointers
    b1 => b1_m
    b2 => b2_m

    if (.not. associated (b1, c1_m%base)) error stop 1_4
    if (.not. associated (b2, c2_m%base)) error stop 2_4

    call printData (b1)
    call printData (b1_m)

    do i = 1, 20, 2
        call printData (b2(i))
        call printData (b2_m(i+1))
    end do

    b1 => b2_m (10)

    call printData (b1)
end
