! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg004a2.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/07/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (poly-dummy-arg-array of
!                               explicit-shape associated with poly-allocatable
!                               of different bounds setting; also the dummy-arg
!                               is used as actual-arg to be associated with
!                               non-poly dummy-arg-array)
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
        class (base(4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(4,1,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

program fArg004a2
use m
    type (child(4,1,20)) :: c1 (4)
    class (base(4)), allocatable :: b1(:)

    c1 = (/(child(4,1,20) (i, 'c1'),i=1,4)/)

    allocate (b1(2:5), source=c1)

    call test1 (b1)

    call test2 (b1)

    contains

    subroutine test1 (b)
        class (base(4)), intent(in) :: b(4)

        call b(1)%print
        call b(2)%print
        call b(3)%print
        call b(4)%print

        call test2 (b)
    end subroutine

    subroutine test2 (b)
        type(base(4)), intent(in) :: b(4)

        call b(1)%print
        call b(2)%print
        call b(3)%print
        call b(4)%print
    end subroutine
end
