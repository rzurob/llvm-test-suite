! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg514a2.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/24/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (argument keyword used for
!                               type bounds)
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
    type base1(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id

        contains

        procedure, pass (b1) :: print2 => printB1B2
        procedure :: print => printBase1
    end type

    type base2(k2,n1)    ! (1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name

        contains

        procedure, pass (b2) :: print2 => printB1B2
        procedure :: print => printBase2
    end type

    contains

    subroutine printB1B2 (b1, b2)
        class (base1(4)), intent(in) :: b1
        class (base2(1,*)), intent(in) :: b2

        call b1%print
        call b2%print
    end subroutine

    subroutine printBase1 (b)
        class (base1(4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printBase2 (b)
        class (base2(1,*)), intent(in) :: b

        print *, b%name
    end subroutine
end module


module m1
use m
    type, extends(base1) :: child1(k3,n2)    ! (4,1,20)
        integer, kind             :: k3
        integer, len              :: n2
        character(kind=k3,len=n2) :: name

        contains

        procedure :: print => printChild1
        procedure, pass(b1) :: print2 => printB1ThenB2
    end type

    type, extends(base2) :: child2(k4)    ! (1,20,4)
        integer, kind :: k4
        integer(k4)   :: id

        contains

        procedure :: print => printChild2
        procedure, pass(b2) :: print2 => printB2ThenB1
    end type

    contains

    subroutine printB1ThenB2 (b1, b2)
        class (child1(4,1,*)), intent(in) :: b1
        class (base2(1,*)), intent(in) :: b2

        call b1%print
        call b2%print
    end subroutine

    subroutine printB2ThenB1 (b1, b2)
        class (base1(4)), intent(in) :: b1
        class (child2(1,*,4)), intent(in) :: b2

        call b2%print
        call b1%print
    end subroutine


    subroutine printChild1 (b)
        class (child1(4,1,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine printChild2 (b)
        class (child2(1,*,4)), intent(in) :: b

        print *, b%name, b%id
    end subroutine
end module


program fArg514a2
use m1
    type (child1(4,1,20)) :: c1 = child1(4,1,20) (1, 'c1')
    type (child2(1,20,4)) :: c2

    type (base1(4)) :: b1
    type (base2(1,20)) :: b2

    c2%name = 'c2'
    c2%id = 2

    b1%id = 10
    b2%name = 'b2'

    call c1%print2 (b2 = child2(1,20,4)('temp', 2))

    call c2%print2 (b1 = c1)

    call b1%print2 (c2)

    call b2%print2 (c1)
end
