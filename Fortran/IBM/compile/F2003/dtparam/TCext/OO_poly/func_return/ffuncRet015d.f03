! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/func_return/ffuncRet015d.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/11/2005
!*
!*  DESCRIPTION                : poly-function return (function results can NOT
!                               be an actual-arg to be associated with a
!                               dummy-arg of INTENT(INOUT) or INTENT(OUT)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (4)
        integer, kind        :: k1
        integer(k1), private :: id

        contains

        procedure :: makeData => makeBaseAlloc
    end type

    contains

    class(base(4)) function makeBaseAlloc (b)
        allocatable makeBaseAlloc
        class(base(4)), intent(in) :: b

        allocate (makeBaseAlloc, source=b)
    end function

    subroutine test1 (b1, b2)
        class(base(4)), allocatable, intent(inout) :: b1
        class(base(4)), allocatable, intent(out) :: b2
    end subroutine
end module

program ffuncRet014d
use m
    class(base(4)), pointer :: b1
    class(base(4)), allocatable :: b2

    allocate (b1)

    call test1 (b1%makeData(), b2)   !<-- illegal
    call test1 (b2, b1%makeData())   !<-- illegal
end