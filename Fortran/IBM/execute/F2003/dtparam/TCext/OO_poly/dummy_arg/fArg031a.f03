! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg031a.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/10/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (dummy-arg used as actual
!                               arg; use assumed-shape arrays)
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

    type, extends (base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class(base(4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(4,1,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine test1 (b)
        type (base(4)), intent(inout) :: b(:)

        call test2 (b(::2))
        call test3 (b(2::2))
    end subroutine

    subroutine test2 (b)
        class (base(4)), intent(inout) :: b(:)

        b(::2)%id = b(::2)%id*2
    end subroutine

    subroutine test3 (b)
        class (base(4)), intent(inout) :: b(3:)

        b%id = b%id + 1
    end subroutine
end module

program fArg031a
use m
    class (base(4)), allocatable :: b1 (:)

    type (child(4,1,20)) :: c1 (10)

    c1%id = (/(i,i=1,10)/)
    c1%name = (/('c1_'//char(ichar('0')+i-1), i=1,10)/)

    allocate (b1(10), source=c1)

    call test1 (b1)

    do i = 1, 10
        call b1(i)%print
    end do
end