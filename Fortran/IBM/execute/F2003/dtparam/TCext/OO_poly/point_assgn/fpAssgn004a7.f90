! GB DTP extension using:
! ftcx_dtp -qnol -qnodeferredlp /tstdev/OO_poly/point_assgn/fpAssgn004a7.f
! opt variations: -qck -ql -qdeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/24/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (pointer assigned to
!*                               type bound functions; nopass binding)
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
        integer(k1)   :: id = 0

        contains

        procedure, nopass :: replicateBase
        procedure :: print => printBase
    end type

    class (base(4)), pointer :: b1_m
    private replicateBase, printBase

    contains

    function replicateBase (b)
        type (base(4)), pointer :: replicateBase
        type (base(4)), intent(in) :: b

        allocate (replicateBase)

        replicateBase = b
    end function

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        print *, b%id
    end subroutine
end module

module m1
use m
    type, extends(base) :: child(n1)    ! (4,20)
        integer, len  :: n1
        character(n1) :: name =''

        contains

        procedure, nopass :: replicateChild
        procedure :: print => printChild
    end type

    contains

    function replicateChild (c)
        type (child(4,20)), pointer :: replicateChild
        type (child(4,*)), intent(in) :: c

        allocate (replicateChild)

        replicateChild = c
    end function

    subroutine printChild (b)
        class (child(4,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

program fpAssgn004a7
use m1
    class (base(4)), pointer :: b_ptr
    type (base(4)) :: b1
    type (child(4,20)) :: c1

    b1%id = 10

    c1 = child(4,20) (20, 'c1')

    b1_m => c1%replicateBase (b1)

    call b1_m%print
    deallocate (b1_m)

    b1_m => c1%replicateBase (c1%base)

    call b1_m%print
    deallocate (b1_m)

    b1_m => c1%replicateChild (c1)

    call b1_m%print
    deallocate (b1_m)

    b_ptr => b1%replicateBase (b1)

    call b_ptr%print
    deallocate (b_ptr)
end
