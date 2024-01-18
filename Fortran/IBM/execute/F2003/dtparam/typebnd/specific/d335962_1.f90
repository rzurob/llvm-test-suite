!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/23/2007
!*
!*  DESCRIPTION                : miscellaneous (defect 335962, reduced TC)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        contains
        procedure, nopass :: str => stringRepBase
    end type

    type, extends(base) :: child
        character(20) :: name
        contains
        procedure, nopass :: str => stringRepChild
    end type

    contains

    character(i) function stringRepChild (b1, i)
        class(base), intent(in) :: b1
        integer, intent(in):: i

        stringRepChild = ''

        select type (b1)
            type is (base)
                stringRepChild = b1%str(b1, i)

            class is (child)
                stringRepChild = b1%name
        end select
    end function

    character(i) function stringRepBase (b1, i)
        class(base), intent(in) :: b1
        integer, intent(in):: i

        stringRepBase = 'base: empty'
    end function
end module

module m2
use m
    type, extends(child) :: thirdGen! (k)
        real(4) :: data(3)

        contains

        procedure, nopass :: str => string3rdGen
    end type

    abstract interface
        character(i) function generalFun (b1, i)
            use m
            class(base), intent(in) :: b1
            integer, intent(in):: i
        end function
    end interface

    procedure(generalFun) string3rdGen
end module

use m2
    class(base), allocatable :: b1, b2(:,:), b3(:)
    character(:), allocatable :: s1

    allocate (b1)

    allocate (b2(2,2), source=child('xlftest 101'))

    allocate (b3(10), source=thirdGen('F2003 team', [1.2, 2.2, 3.2]))

    s1 = b3%str(b1, 20)

    print *, s1
end

character(i) function string3rdGen (b1, i)
use m2, only: base, child, thirdGen
    class(base), intent(in) :: b1
    integer, intent(in):: i

    select type (b1)
        class is (base)
            string3rdGen = b1%str(b1, i)

        class is (thirdGen)
            write (string3rdGen, '(a, 3g10.3)') b1%name, b1%data

    end select

end function
