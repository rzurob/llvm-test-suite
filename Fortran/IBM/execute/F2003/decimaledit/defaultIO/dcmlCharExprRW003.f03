! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/07/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Use of the namelist for the format control with
!                               DECIMAL= specifier. test is for C928: format or
!                               namelist MUST also present if DECIMAL= appears
!                               in data transfer statement.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer :: id
        real :: d(2)
    end type

    type, extends(base) :: child
        character(20) :: name
        complex :: cx
    end type
end module


program dcmlCharExprRW003
use m
    type(base), allocatable :: b1(:)

    type(child) c1

    namelist /nml1/ b1, c1

    allocate (b1(10), source=(/(base(i, (/i, i*2/)), i=1,10)/))

    c1 = child (100, 1.5, 'xlftest F2003', cmplx(-1, -10))

    write (*, nml1, decimal='COMMA')

    write (nml=nml1, unit=*, delim='APOSTROPHE', decimal='POINT')
end
