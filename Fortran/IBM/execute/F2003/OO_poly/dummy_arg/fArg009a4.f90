!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/18/2005
!*
!*  DESCRIPTION                : argument association (VALUE attribute: the
!                               entity has the initial value of actual-arg)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        real(8), pointer :: data(:) => null()
    end type

    type, extends(base) :: child
        character(20) :: name
    end type

    interface printData
        module procedure printData
        module procedure printDataChild
    end interface

    contains

    subroutine printData (b)
        type(base), value :: b

        if (associated(b%data)) write (*, '(5f10.2)') b%data
    end subroutine

    subroutine printDataChild (x)
        type(child), value :: x

        if (associated(x%data)) then
            write (*, '(5f10.2)', advance='no') x%data
            write (*, *) '; ', x%name
        else
            write (*, *) x%name
        end if
    end subroutine


    subroutine test1 (x)
        class(*) :: x

        select type(x)
            class is (base)
                call printData (x)

            type is (child)
                call printData (x)

            class default
                print *, 'unknown type'
        end select
    end subroutine
end module

program fArg009a4
use m
    class (base), allocatable :: b1, b2(:)

    !! test 1
    allocate (b1, source=child(name='xlftest'))

    call test1(b1)

    !! test 2
    allocate (b1%data(2), source=(/1.5_8, 2.5_8/))

    call test1(b1)

    !! test 3

    allocate (b2(2))

    allocate (b2(2)%data(3), source=1.25_8)

    call test1 (b2(2))
    call test1 (b2(1))
end
