! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/17/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 324249)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type string10! (n)
        integer :: n = 10

        character(10), allocatable :: rep
    end type

    type base4! (k)
        integer(4) :: id
        type(string10), allocatable :: name(:)
    end type
end module

program dtparamConstr055
use m
    type(string10), pointer :: names(:)

    type (base4), pointer :: b1

    allocate (names(-10:10))

    names = (/(string10(10, 'xlftest '//achar(10+i)), i=-10,10)/)

    allocate (b1, source=base4(100, names))

    !! verify b1
    if (b1%id /= 100) error stop 1_4

    if (.not. allocated(b1%name)) error stop 2_4

    if ((lbound(b1%name,1) /= -10) .or. (ubound(b1%name,1) /= 10)) &
            error stop 3_4

    if (b1%name(0)%n /= 10) error stop 4_4

    do i = 0, 20
        if (.not. allocated(b1%name(i-10)%rep)) error stop 5_4

        if (b1%name(i-10)%rep /= 'xlftest '// achar(i)) error stop 6_4
    end do
end
