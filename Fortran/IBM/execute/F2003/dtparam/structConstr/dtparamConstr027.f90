! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/07/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constructor)
!                               Case: Use of different forms of expressions for
!                               data component in structure constructor:
!                               function reference that returns array.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module randomNumStore
    real(8) :: rand8(1000)
end module

module m
    type base (k, n, l)
        integer, kind :: k
        integer, len :: n, l

        real(k) :: data(n)
        character(l) :: name
    end type

    type, extends(base) :: child
        integer(k/3) :: id
    end type

    class(base(8,:,:)), allocatable :: b1

    abstract interface
        function getRandVal8 (index, howmany)
            integer, intent(in) :: index, howmany

            real(8) getRandVal8 (howmany)
        end function
    end interface
end module

program dtparamConstr027
use m
use randomNumStore
    type (child(4, 37, 25)) :: c1

    logical(4), external :: precision_r4, precision_r8

    procedure(getRandVal8) getRand8

    call setRand8

    allocate (b1, source=child(8, 22, 30)(getRand8(10, 22), 'b1 of type child',&
            countLength('b1 of type child')))

    c1 = child(4, 37, 25)(getRand8(500, 37), name=repeat('xlftest',4), &
            id = -1*countLength(repeat('xlftest',4)))


    !! verify
    do i = 1, 22
        if (.not. precision_r8(b1%data(i), rand8(9+i))) error stop 1_4
    end do

    do i = 1, 37
        if (.not. precision_r4(c1%data(i), real(rand8(499+i), 4))) &
                error stop 2_4
    end do

    if (b1%name /= 'b1 of type child') error stop 3_4
    if (c1%name /= 'xlftestxlftestxlftestxlft') error stop 4_4

    if (c1%id /= -28) error stop 5_4

    select type (b1)
        type is (child(8,*,*))
            if (b1%id /= 16) error stop 6_4

        class default
            error stop 7_4
    end select

    contains

    integer(8) function countLength (c)
        character(*), intent(in) :: c

        countLength = len (c)
    end function
end


function getRand8 (index, howmany)
use randomNumStore
    integer, intent(in) :: index, howmany

    real(8) getRand8 (howmany)

    getRand8 = rand8(index:index+howmany-1)
end function


subroutine setRand8
use randomNumStore
    call random_number(rand8)
end subroutine
