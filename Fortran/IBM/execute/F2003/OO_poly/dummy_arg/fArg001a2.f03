! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/30/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (procedure as the actual
!*                               argument)
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
        integer*4 :: id = -1
    end type

    type, extends(base) :: child
        character*20 :: name = 'no-name'
    end type

    contains

    integer function add (b1, b2)
        type (base), intent(in) :: b1, b2

        add = b1%id + b2%id
    end function

    integer function subtract (b1, b2)
        type (base), intent(in) :: b1, b2

        subtract = b1%id - b2%id
    end function
end module

module m1
use m
    contains

    integer function operateB1B2 (b1, b2, op)
        class (base), intent(in) :: b1, b2

        interface
            integer function op (bb1, bb2)
            use m
                type (base), intent(in) :: bb1, bb2
            end function
        end interface

        operateB1B2 = op (b1, b2)
    end function
end module

program fArg001a2
use m1
    type (base) :: b1
    type (child) :: c1

    b1%id = 1

    c1 = child (2, 'c1')

    if (operateB1B2 (child(1,'test1'), child(2,'test2'), op = add) /= 3) &
            error stop 1_4


    if (operateB1B2 (b1 = b1, b2 = child(1,'abc'), op = subtract) /= 0) &
            error stop 2_4


    if (operateB1B2 (b1, b2=c1, op = add) /= 3) error stop 3_4

    if (operateB1B2 (b1, b2=c1%base, op = subtract) /= -1) error stop 4_4
end
