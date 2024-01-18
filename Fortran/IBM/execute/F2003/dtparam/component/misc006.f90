!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/24/2006
!*
!*  DESCRIPTION                : miscellaeous (defect 315385)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base10! (n)
        integer :: n = 10
        real :: data(10)
        procedure(getAll), pointer, pass(b2) :: all => null()
    end type

    contains

    function getAll (b1, b2, b3)
        real, pointer, dimension(:) :: getAll
        class (base10), intent(in) :: b1, b2, b3

        allocate(getAll(b1%n+b2%n+b3%n), source=(/b1%data, b2%data, b3%data/))
    end function
end module

program dtparamProcComp008
use m
    real, pointer :: r2(:)

    logical(4), external :: precision_r4

    class(base10), allocatable :: b1(:)

    allocate(base10 :: b1(5))

    do i = 1, 5
        b1(i)%data = (/(i*1.0e1+j, j=0, 9)/)
        b1(i)%all => getAll
    end do

    r2 => b1(2)%all (b1(1), b1(3))

    !! verify r2
    if (size(r2) /= 30) error stop 1_4

    do i = 1, 30
        if (.not. precision_r4(r2(i), 9.0+i*1.0)) error stop 2_4
    end do

end
