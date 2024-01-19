! GB DTP extension using:
! ftcx_dtp -qnock -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/implicit/fimplct006.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/16/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : IMPLICIT (implicit poly entities as function
!*                               return results)
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
        procedure, non_overridable :: replicate => bReplicateBase
    end type

    type (base(4)) :: b1_m = base(4)(-1)
    contains

    subroutine printBase (b)
        implicit class (base(4)) (b)
        intent(in) b

        print *, b%id
    end subroutine

    function bReplicateBase (b)
        implicit class (base(4)) (b)

        intent(in) :: b
        pointer bReplicateBase

        allocate (bReplicateBase, source=b)
    end function
end module

module m1
use m, only : base
    type, extends (base) :: child(n1)    ! (4,20)
        integer, len  :: n1
        character(n1) :: name

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printChild (b)
        class (child(4,*)) :: b
        intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

program fimplct006
use m, only : b1_m
use m1
    implicit class (base(4)) (b)

    pointer b_ptr

    type (child(4,20)) :: c1 = child(4,20) (10, name = 'c1')

    type, extends (child) :: gen3    ! (4,20)
        logical(k1) :: flag = .false.
    end type

    type (gen3(4,20)) g1

    g1 = gen3(4,20) (name = 'g3', id = 100)

    b_ptr => c1%replicate()

    call b_ptr%print

    deallocate (b_ptr)

    b_ptr => g1%replicate()

    call b_ptr%print

    deallocate (b_ptr)

    b_ptr => b1_m%replicate()

    call b_ptr%print

    deallocate (b_ptr)
end
