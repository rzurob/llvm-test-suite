! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/31/2005
!*
!*  DESCRIPTION                : dummy_arg (recursive calls between two
!                               subroutines)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer, allocatable :: id
    end type

    integer, parameter :: recurseNo = 5000
    integer, save :: howManyCalls = 0


    contains

    recursive subroutine printX (x)
        implicit none
        class (*), intent(in) :: x(:)

        select type (x)
            type is (integer)
                call printInt (x)
            type is (character(*))
                call printChar (x)
            type is (base)
                call printBase (x)
            class default
                print *, 'not previously defined types'
        end select
    end subroutine

    recursive subroutine printInt (i)
        implicit none
        integer, intent(in) :: i(:)

        if (howManyCalls < recurseNo) then

            howManyCalls = howManyCalls + 1

            call printX (i)
        else
            print *, i
            howManyCalls = 0
        end if
    end subroutine

    recursive subroutine printChar (c)
        implicit none
        character(*), intent(in) :: c(:)

        integer :: callNo = 0

        if (callNo < recurseNo) then
            callNo = callNo + 1

            call printX (c)
        else
            print *, c

            callNo = 0
        end if
    end subroutine

    recursive subroutine printBase (b)
        implicit none
        type (base), intent(in) :: b(:)

        integer, save :: callNo = 0
        type (base) localTemp(size(b))
        integer i

        if (callNo < recurseNo) then
            callNo = callNo + 1

            localTemp = b

            do i = 1, size(localTemp)
                if (allocated(localTemp(i)%id)) then
                    localTemp(i)%id = localTemp(i)%id + 1
                end if
            end do

            call printX (localTemp)
        else
            do i = 1, size(b)
                if (allocated(b(i)%id)) print *, b(i)%id
            end do

            callNo = 0
        end if
    end subroutine
end module

program fArg041a
use m
    character(10), parameter :: c1(3) = (/'test 01', 'test 02', 'test 03'/)

    class (base), pointer :: b1(:)

    allocate (b1(10), source=(/(base(i*10), i=1,10)/))

    call printX ((/100, 200/))

    call printX (c1)

    call printX (b1)

    call printX ((/base(1), base(2), base(3)/))
end