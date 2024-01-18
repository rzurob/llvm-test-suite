! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/3/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (test the basic binding
!                               calls for assumed-size array dummy arg)
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
        integer id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character(15) name

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

program fArg014a1
use m
    class (base), allocatable :: b1(:)

    allocate (b1(3), source=(/child(1,'test1'), child(2,'test2'), &
        child(3,'test3')/))


    call abc (b1)

    call abc (b1(1:3:2))

    call abc ((/base(10), base(20), base(30)/))

    call abc ((/child (100, 'xlftest 100'), child(200, 'xlftest 200')/))

    contains

    subroutine abc (b)
        class (base) b(*)

        call b(1)%print
        call b(2)%print
    end subroutine
    end
