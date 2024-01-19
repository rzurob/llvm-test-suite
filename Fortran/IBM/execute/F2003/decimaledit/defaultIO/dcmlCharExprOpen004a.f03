! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/02/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Test that the open statement has no effect on
!                               the decimal edit mode if the DECIMAL= specifier
!                               does not appear in the open statement; test
!                               the list-directed READ statement
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        real(8) :: d1(10)
        character(10) :: name
        complex(4) :: cx
    end type

    contains

    elemental logical function baseTypeEqual (b1, b2)
        type (base), intent(in) :: b1, b2

        real(8) delta8
        real(4) delta4

        delta8 = 5.0d-16
        delta4 = 5.0e-7

        baseTypeEqual = b1%name == b2%name

        do i = 1, 10
            baseTypeEqual = baseTypeEqual .and. &
                abs(b1%d1(i) - b2%d1(i)) <= abs(b1%d1(i)+b2%d1(i))*delta8

            if (.not. baseTypeEqual) exit
        end do

        baseTypeEqual = baseTypeEqual .and. &
            abs(real(b1%cx)-real(b2%cx)) <= abs(real(b1%cx)+real(b2%cx))*delta4

        baseTypeEqual = baseTypeEqual .and. &
            abs(aimag(b1%cx)-aimag(b2%cx)) <= abs(aimag(b1%cx)+aimag(b2%cx))*delta4
    end function
end module

program dcmlCharExprOpen004a
use m
    real(4) :: r1, r2

    type (base), allocatable :: b1(:), b2(:)

    character(10) decMod, names(3)
    logical(4), external :: precision_r4

    character(10) commaMode
    common /blk1/ commaMode

    r1 = 1.0
    names = (/'123', '321', 'zzz'/)

    allocate (b2(2:4))

    allocate(b1(3), source = (/(base((/(j, j=1, 10)/), names(i), &
            cmplx(i,i,8)), i=1,3)/))

    open (10, file='dcmlCharExprOpen004a.out')

    write (10, *, decimal='COMMA') r1, b1

    backspace 10

    call updateDecimalMode (10)

    read (10, *) r2, b2

    if (any(.not. baseTypeEqual(b1, b2))) error stop 1_4
    if (.not. precision_r4 (r1,r2)) error stop 2_4

    call updateSignMode (10)

    rewind (10)

    read (10, *) r2, b2

    if (any(.not. baseTypeEqual(b1, b2))) error stop 3_4
    if (.not. precision_r4 (r1,r2)) error stop 4_4

    backspace 10
    call updateDelimMode (10)
    call updatePadMode(10)
    call updateRoundMode(10)

    read (10, *) r2, b2

    if (any(.not. baseTypeEqual(b1, b2))) error stop 5_4
    if (.not. precision_r4 (r1,r2)) error stop 6_4
end

subroutine updateDecimalMode (unit)
    integer, intent(in) :: unit

    character(10) commaMode
    common /blk1/ commaMode

    open (unit, decimal=commaMode)

end subroutine

subroutine updateSignMode (unit)
    integer, intent(in) :: unit

    open (unit, sign='PLUS')
end subroutine

subroutine updateDelimMode (unit)
    integer, intent(in) :: unit

    open (unit, delim='APOSTROPHE')
end subroutine

subroutine updatePadMode (unit)
    integer, intent(in) :: unit

    open (unit, pad='NO')
end subroutine

subroutine updateRoundMode (unit)
    integer, intent(in) :: unit

    open (unit, round='UP')
end subroutine


block data
    character(10) commaMode
    common /blk1/ commaMode
    data commaMode /'COMMA'/
end block data
