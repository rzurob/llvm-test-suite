!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/02/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Test that the open statement has no effect on
!                               the decimal edit mode if the DECIMAL= specifier
!                               does not appear in the open statement; also test
!                               the namelist in write statement
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        real(8) :: d1(10)
        character(10) :: name
        complex(4) :: cx
    end type
end module

program dcmlCharExprOpen004
use m
    real(4) :: r1

    type (base) :: b1(3)

    character(10) decMod, names(3)

    namelist /nml1/ r1, b1

    r1 = 1.0
    names = (/'123', '321', 'zzz'/)

    b1 = (/(base((/(j, j=1, 10)/), names(i), cmplx(i,i,8)), i=1,3)/)

    open (100, file='dcmlCharExprOpen004.out', decimal='COMMA')

    write(100, nml=nml1)

    call updateSignMode (100)

    inquire (100, decimal=decMod)

    if (decMod /= 'COMMA') error stop 1_4

    write(100, nml=nml1)

    call updateDelimMode(100)

    write(100, nml=nml1)

    inquire (100, decimal=decMod)

    if (decMod /= 'COMMA') error stop 2_4

    call updatePadMode (100)

    inquire (100, decimal=decMod)

    if (decMod /= 'COMMA') error stop 3_4

    write(100, nml=nml1)

    call updateRoundMode(100)

    write(100, nml=nml1)

    inquire (100, decimal=decMod)

    if (decMod /= 'COMMA') error stop 4_4

    close(100)
end

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
