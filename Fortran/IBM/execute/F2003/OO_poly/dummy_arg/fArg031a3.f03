! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/14/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (dummy_arg used as
!                               actual-arg; test squence association)
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
    type base
        integer*4 :: id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character*20 :: name

        contains

        procedure :: print => printChild
    end type

    private internalT

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine test1 (c)
        class (child), intent(in) :: c(3)

        call internalT (c)
    end subroutine

    subroutine test2 (c)
        type (child), intent(in) :: c(3)

        call internalT (c)
    end subroutine

    subroutine internalT (b)
        class (base), intent(in) :: b(3)

        call b(1)%print
        call b(2)%print
        call b(3)%print
    end subroutine
end module

program fArg031a3
use m
    type (child) :: c1 (3)

    c1 = (/child(1,'c1_1'), child(2,'c1_2'), child(3,'c1_3')/)


    call test1 (c1)

    call test1 ((/child(10, 'temp1'), child(20, 'temp2'), child(30, 'temp3')/))

    call test2 (c1)

    call test2 ((/child(10, 'temp1'), child(20, 'temp2'), child(30, 'temp3')/))
end