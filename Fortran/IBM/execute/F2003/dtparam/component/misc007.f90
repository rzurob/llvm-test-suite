!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/25/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 315447)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base14! (k)
        real(4) :: data

        contains

        procedure, pass(b1) :: calc4 => addAll4
!        procedure, pass(b1) :: calc8 => addAll8
    end type

    type base18! (k)
        real(8) :: data

        contains

!        procedure, pass(b1) :: calc4 => addAll4
        procedure, pass(b1) :: calc8 => addAll8
    end type

    type base2_20! (n)
        integer :: n = 20

        real :: data(20)

        contains

        procedure, pass(b2) :: calc4 => addAll4
        procedure, pass(b2) :: calc8 => addAll8
    end type

    contains

    subroutine addAll4 (b1, b2)
        class (base14), intent(inout) :: b1
        class (base2_20), intent(in) :: b2

        b1%data = sum (b2%data)
    end subroutine

    subroutine addAll8 (b1, b2)
        class (base18), intent(inout) :: b1
        class (base2_20), intent(in) :: b2

        b1%data = product(real(b2%data, 8))
    end subroutine
end module

program dtparamPassObj003
use m
    class (base14), allocatable :: b1_1(:)
    class (base2_20), allocatable :: b2_1
    class (base18), pointer :: b1_2(:)

    logical(4), external :: precision_r4, precision_r8

    allocate (b1_1(2), b1_2(10))

    allocate (base2_20 :: b2_1)

    b2_1%data = (/(i*1.0, i=1, 20)/)

    call b1_1(1)%calc4 (b2_1)
    call b2_1%calc4(b1_1(2))

    call b1_2(1)%calc8 (b2_1)
    call b2_1%calc8 (b1_2(10))

    !! verify
    if (.not. precision_r4 (b1_1(1)%data, b1_1(2)%data)) error stop 1_4

    if (.not. precision_r8 (b1_2(1)%data, b1_2(10)%data)) error stop 2_4

    write (*, '(f10.2, g15.5)') b1_1(1)%data, b1_2(1)%data
end
