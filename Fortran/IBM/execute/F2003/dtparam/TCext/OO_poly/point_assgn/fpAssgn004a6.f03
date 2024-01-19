! GB DTP extension using:
! ftcx_dtp -ql -qnodeferredlp -qreuse=base /tstdev/OO_poly/point_assgn/fpAssgn004a6.f
! opt variations: -qck -qnol -qdeferredlp -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/23/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (pointer assigned to
!*                               function return results; function is
!*                               type-bound)
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
        integer(k1)   :: id

        contains

        procedure :: clone => produceBasePtr
        procedure :: print => printBase
    end type

    contains

    function produceBasePtr (b)
        class (base(20,4)), pointer :: produceBasePtr
        class (base(*,4)), intent(in) :: b

        allocate (produceBasePtr)

        produceBasePtr%id = b%id
    end function

    subroutine printBase (b)
        class (base(*,4)), intent(in) :: b

        print *, b%id
    end subroutine
end module

module m1
use m, only : base

    type, extends (base) :: child    ! (20,4)
        character(n1) :: name = ''

        contains

        procedure :: print => printChild
        procedure :: clone => produceChildPtr
    end type

    contains

    subroutine printChild (b)
        class (child(*,4)), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    function produceChildPtr (b)
        class (base(20,4)), pointer :: produceChildPtr
        class (child(*,4)), intent(in) :: b

        type (child(20,4)), pointer :: temp

        allocate (temp)

        temp = child(20,4) (b%id, b%name)

        produceChildPtr => temp
    end function
end module

program fpAssgn004a6
use m1, only : base, child
    type (base(20,4)) :: b1
    type (child(20,4)) :: c1

    class (base(20,4)), pointer :: b_ptr


    c1 = child(20,4) (10, 'c1')

    b_ptr => c1%clone()

    call b_ptr%print

    deallocate (b_ptr)

    b1%id = 20

    b_ptr => b1%clone()

    call b_ptr%print

    deallocate (b_ptr)
end
