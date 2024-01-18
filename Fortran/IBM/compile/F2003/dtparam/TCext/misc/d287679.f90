! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/misc/d287679.f
! opt variations: -qnol

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/07/2005
!*
!*  DESCRIPTION                : argument association (C1232: actual-arg is
!                               array section and associated dummy-arg is
!                               volatile, then it MUST be assumed-shape array)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)   :: id
    end type

    contains

    subroutine test1 (b)
        class (base(*,4)), intent(inout), volatile :: b (3)
    end subroutine

    subroutine test2 (b)
        type (base(*,4)), intent(inout), volatile :: b (3)

        print *, b
    end subroutine
end module

program fArg023d
use m
    type (base(20,4)) :: b1(10)

    b1 = (/(base(20,4)(i), i=1,10)/)

    call test1 (b1(2:6:2))      !<-- can't call this way

    call test2 (b1 (8:))        !<-- can't call this way

    contains

    subroutine ttt (x)
        class(base(*,4)), intent(inout):: x(:)

        call test1 (x)          !<-- can't call this way
    end subroutine
end
