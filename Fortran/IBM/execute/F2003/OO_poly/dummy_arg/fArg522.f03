! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/11/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (cross testing with select
!                               type construct)
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
        integer(8) :: id
    end type

    type, extends(base) :: child
        character(15) :: name
    end type

    contains

    subroutine printData1 (b)
        class (base), intent(in) :: b

        select type (b)
            type is (base)
                print *, b%id
            type is (child)
                print *, b%id, b%name
            class default
                error stop 1_4
        end select
    end subroutine

    subroutine printDataArray (b)
        class (base), intent(in) :: b(:)

        do i = 1, size(b)
            call printData1 (b(i))
        end do
    end subroutine
end module

program fArg522
use m
    class (*), allocatable :: x1, x2(:)

    allocate (x1, source=child(100_8, 'x1 scalar'))

    select type (x1)
        class is (base)
            call printData1 (x1)
        class default
            error stop 10_4
    end select

    allocate (x2(0:1), source=(/child(1_8,'x1 array 1'), child(2_8,'x1 array 2')/))

    select type (x2)
        class is (base)
            call printDataArray(x2)
        class default
            error stop 11_4
    end select
end
