! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg033a.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/14/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (actual-arg changed during
!                               the execution of the procedure)
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

    type (child(4,1,20)) :: c1 = child(4,1,20) (1, 'c1')

    contains

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(4,1,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine test1 (b)
        class (base(4)), target, intent(inout) :: b

        c1%id = c1%id * 10

        call b%print
    end subroutine
end module


program fArg033a
use m
    class (base(4)), allocatable :: b1

    allocate (b1, source=child(4,1,20) (1, 'b1'))

    call test1 (c1)

    call test2 (b1)

    call b1%print

    call c1%print

    contains

    subroutine test2 (b)
        class (base(4)), target, intent(out) :: b

        b1%id = 100

        call b%print
    end subroutine
end
