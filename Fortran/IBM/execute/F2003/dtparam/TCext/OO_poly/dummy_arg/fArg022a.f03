! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg022a.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/02/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (poly-actual-arg
!                               associated with nonpoly-dummy-arg with
!                               INTENT(OUT) attribute)
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
        procedure, non_overridable :: assgnId => assignID2Base
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base(4)), intent (in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(4,1,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine assignID2Base (b, id)
        class (base(4)), intent(inout) :: b

        b%id = id
    end subroutine
end module

program fArg022a
use m
    interface assignment(=)
        subroutine base2Base (b1, b2)
        use m
            type (base(4)), intent(out) :: b1
            class (base(4)), intent(in) :: b2
        end subroutine
    end interface

    class (base(4)), allocatable :: b1(:)

    type (child(4,1,20)) :: c1 = child(4,1,20) (100, 'c1_static')

    allocate (b1(3), source=(/child(4,1,20)(1, 'b1_1'), child(4,1,20)(2, 'b1_2'), &
                child(4,1,20)(3,'b1_3')/))


    !
    b1(1) = b1(3)

    b1(2) = c1

    call b1(1)%print
    call b1(2)%print
    call b1(3)%print
end

subroutine base2Base (b1, b2)
use m
    type (base(4)), intent(out) :: b1
    class (base(4)), intent(in) :: b2

    call b1%assgnID(b2%id)
end subroutine
