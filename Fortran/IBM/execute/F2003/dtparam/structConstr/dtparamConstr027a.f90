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
    real(8) :: rand8(10000)
end module

module m
    type base (k, dim1, dim2, l)
        integer, kind :: k
        integer, len :: dim1, dim2, l

        real(k) :: data(dim1,dim2)
        character(l) :: name
    end type

    type, extends(base) :: child
        integer(k/3) :: id
    end type

    class(base(8,:,:,:)), allocatable :: b1

    abstract interface
        function getRandVal8 (index, dim1, dim2)
            integer, intent(in) :: index, dim1, dim2

            real(8) getRandVal8 (dim1, dim2)
        end function
    end interface
end module

program dtparamConstr027a
use m
use randomNumStore
    type (child(4, 70, 80, 25)) :: c1

    logical(4), external :: precision_r4, precision_r8

    procedure(getRandVal8) getRand8

    call setRand8

    allocate (b1, source=child(8, 60, 40, 30)(getRand8(10, 60, 40), &
            repeat('xlftest',4), countLength(repeat('xlftest',4))))

    c1 = child(4,70,80,25)(data=getRand8(500,70,80), name=repeat('xlftest',4), &
            id = -1*countLength(repeat('xlftest',4)))


    !! verify
    k = 2409

    do j = 1, 40
        do i = 1, 60
            if (.not. precision_r8(b1%data(i,j), rand8(k))) error stop 1_4

            k = k -1
        end do
    end do

    k = 5600+499

    do j = 1, 80
        do i = 1, 70
            if (.not. precision_r4(c1%data(i, j), real(rand8(k), 4))) &
                error stop 2_4

            k = k -1
        end do
    end do

    if (b1%name /= 'xlftestxlftestxlftestxlftest') error stop 3_4
    if (c1%name /= 'xlftestxlftestxlftestxlft') error stop 4_4

    if (c1%id /= -28) error stop 5_4

    select type (b1)
        type is (child(8,*,*,*))
            if (b1%id /= 28) error stop 6_4

        class default
            error stop 7_4
    end select

    contains

    integer(8) function countLength (c)
        character(*), intent(in) :: c

        countLength = len (c)
    end function
end


function getRand8 (index, dim1, dim2)
use randomNumStore
    integer, intent(in) :: index, dim1, dim2

    real(8) getRand8 (dim1, dim2)

    getRand8 = reshape(rand8(index+dim1*dim2-1:index:-1), (/dim1, dim2/))
end function


subroutine setRand8
use randomNumStore
    call random_number(rand8)
end subroutine
