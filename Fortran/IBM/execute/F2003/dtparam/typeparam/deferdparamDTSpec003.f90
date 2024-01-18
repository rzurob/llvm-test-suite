!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/10/2006
!*
!*  DESCRIPTION                : dtparam (section 4.2: type parameters)
!                               Case: Deferred type-parameters in
!                               declaration-type-spec: type parameter query used
!                               for associated pointer and allocated
!                               allocatables.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (l, n)
        integer, len :: l, n

        character(l) :: name = 'default'
        integer :: ids(n) = -1
    end type
end module

program deferdparamDTSpec003
use m
    class(base(:,:)), pointer :: b1(:)
    type (base(:,:)), allocatable :: b2

    allocate (base(20, 10):: b1(10))

    allocate (base(50, n=200) :: b2)

    if ((b1%l /= 20) .or. (b1%n /= 10)) error stop 1_4
    if ((b2%l /= 50) .or. (b2%n /= 200)) error stop 2_4

    b1%name = (/('b1 in main program:'//char(ichar('0')+i)//'truncated', &
            i = 0, 9)/)

    b2%name = 'b2 in main program with 50 chars for name'

    do i = 1, 10
        b1(i)%ids = (/(i*100 + k, k=1, 10)/)
    end do

    !! print out the values
100 format (a, /, (10i5))

    write (*, 100) b2

    do i = 1, 10
        write (*, 100) b1(i)%name, b1(i)%ids
    end do
end
