! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/13/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: parameterized derived type with default
!                               values for type parameters used in function
!                               results.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k = 8
        integer, len :: n = 35

        real(k), private :: data(n) = -1.0d0
    end type

    type, extends(base) :: child (l)
        integer, len :: l = 20

        character(l), private :: name = 'default'
    end type

    interface base
        module procedure produceBase
    end interface

    interface child
        module procedure produceChildObj
    end interface

    contains

    type (base) function produceBase (d)
        real(8), intent(in) :: d(35)

        produceBase%data = d
    end function

    type (child) function produceChildObj (d, name)
        real(8), intent(in) :: d(35)
        character(*), intent(in) :: name

        produceChildObj%data = d
        produceChildObj%name = name
    end function

    real(8) function getData (b, i)
        class (base), intent(in) :: b
        integer, intent(in) :: i

        if ((i < 1) .or. (i > 35)) stop 10

        getData = b%data(i)
    end function

    function getName (c)
        class(child), intent(in) :: c
        character(c%l) getName

        getName = c%name
    end function
end module

program dtparamDefVal007
use m
    class (base), allocatable :: b1(:)

    logical(4), external :: precision_r8

    allocate (child :: b1(10))

    if (.not. precision_r8(getData(b1(5), 15), -1.0d0)) error stop 1_4

    select type (x => b1(6))
        type is (child(n=*,l=*))
            if (getName(x) /= 'default') error stop 2_4

        class default
            error stop 3_4
    end select

    deallocate (b1)

    allocate (b1(10000), source=(/(child((/(i*1.0d3+j, j=1,35)/), 'xlftest'), &
                i=1, 10000)/))


    !! verify
    do i = 1, 10000
        do j = 1, 35
            if (.not. precision_r8 (getData(b1(i), j), i*1.0d3+j)) &
                    error stop 4_4
        end do

        select type (x => b1(i))
            class is (child(n=*,l=*))
                if (getName(x) /= 'xlftest') error stop 5_4

            class default
                error stop 6_4
        end select
    end do
end
