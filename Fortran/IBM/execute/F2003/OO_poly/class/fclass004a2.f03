! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/01/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : class keyword (RHS as polymorphic data in an
!*                               intrinsic assignment; use arrays)
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
    end type

    type, extends(base) :: child
        character*20 :: name
    end type

    contains

    character(2) function int2Char (i)
        integer*4, intent(in) :: i

        write (int2Char, '(i2.2)') i
    end function
end module

program fclass004a2
use m
    interface operator (==)
        elemental logical function baseEqInt (b, i)
        use m
            type (base), intent(in) :: b
            integer*4, intent(in) :: i
        end function
    end interface

    class (base), pointer :: b_ptr(:)
    type (child), target :: c1 (3:12)

    type (base), allocatable :: b1(:)

    allocate (b1(10))

    c1 = (/(child (i, 'c1_'//int2Char(i)), i=3,12)/)

    b_ptr => c1

    b1 = b_ptr

    print *, b1

    if (.not. all (b1 == (/(i, i=3,12)/))) error stop 1_4

    b1 = b_ptr (10)

    print *, b1

    if (.not. all (b1 == 10)) error stop 2_4
end

elemental logical function baseEqInt (b, i)
use m
    type (base), intent(in) :: b
    integer*4, intent(in) :: i

    baseEqInt = (b%id == i)
end function
