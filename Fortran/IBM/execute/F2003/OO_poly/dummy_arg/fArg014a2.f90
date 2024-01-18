! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/15/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (assumed-size array in
!                               select type construct)
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
    end type

    type, extends(base) :: child
        character(15) name
    end type
end module

program fArg014a2
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
        class (base), intent(in) :: b(*)

        select type (b)
            type is (base)
                print *, b(1), b(2)
            type is (child)
                print *, b(1), b(2)
            class default
                error stop 1_4
        end select
    end subroutine
    end
