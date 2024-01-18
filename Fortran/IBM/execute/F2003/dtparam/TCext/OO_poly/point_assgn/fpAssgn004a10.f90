! GB DTP extension using:
! ftcx_dtp -qck -qnodeferredlp /tstdev/OO_poly/point_assgn/fpAssgn004a10.f
! opt variations: -qnock -qdeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/29/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (poly-pointer assigned
!*                               to a type-bound function)
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
    type base(k1,n1)    ! (1,20)
        integer, kind             :: k1
        integer, len              :: n1
        character(kind=k1,len=n1) :: name

        contains

        procedure :: replicate => replicateBase
        procedure :: print => printBase
    end type

    type, extends(base) :: child(k2)    ! (1,20,4)
        integer, kind :: k2
        integer(k2)   :: id

        contains

        procedure :: replicate => replicateChild
        procedure :: print => printChild
    end type

    type (child(1,20,4)), target :: c1_m = child(1,20,4) ('c1_m', 10)

    contains

    subroutine printBase (b)
        class (base(1,*)), intent(in) :: b

        print *, b%name
    end subroutine

    subroutine printChild (b)
        class (child(1,*,4)), intent(in) :: b

        print *, b%name, b%id
    end subroutine

    function replicateBase (b)
        class (base(1,*)), intent(in) :: b
        class (base(1,20)), pointer :: replicateBase

        allocate (replicateBase)

        replicateBase%name = b%name
    end function

    function replicateChild (b)
        class (base(1,20)), pointer :: replicateChild
        class (child(1,*,4)), intent(in) :: b

        type (child(1,20,4)), pointer :: tmp

        allocate (tmp)

        tmp%name = b%name
        tmp%id = b%id

        replicateChild => tmp
    end function
end module

program fpAssgn004a10
use m
    class (base(1,20)), pointer :: b1

    class (child(1,20,4)), allocatable, target :: c1

    allocate (c1)

    c1%name = 'c1'
    c1%id = 20

    b1 => c1

    b1 => b1%replicate()

    call b1%print

    deallocate (b1, c1)

    b1 => c1_m

    b1 => b1%replicate()

    call b1%print

    deallocate (b1)
end
