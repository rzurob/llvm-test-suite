!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 02/23/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constr)
!                               Case: Still the procedure target in the
!                               structure constructor; use procedures with
!                               implicit interfaces.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)
        procedure(real(k)), pointer, nopass :: calc
    end type
end module

program dtparamConstr004a
use m
    procedure(double precision) :: calc8, exp8
    procedure(real(4)) :: calc4

    type(base(8,30)) :: b1
    type(base(4,21)) :: b2

    logical(4), external :: precision_r4, precision_r8

    b1 = base(8, 30) ((/(log(i*1.0d0), i=1,30)/), calc8)
    b2 = base(4,21) ((/(i*1.0e0, i=1, 21)/), calc4)

    if (.not. precision_r8(b1%calc(b1%data, b1%n, exp8), 4.65d2)) error stop 1_4

    if (.not. precision_r4(b2%calc(b2%data, b2%n), 5.1090942e19)) error stop 2_4
end

double precision function calc8 (d1, nsize, proc)
    real(8), intent(in) :: d1(nsize)
    integer, intent(in) :: nsize
    procedure(real(8)) proc

    calc8 = 0.d0

    do i = 1, nsize
        calc8 = calc8 + proc(d1(i))
    end do
end function


double precision function exp8 (d1)
    double precision, intent(in) :: d1

    exp8 = exp(d1)
end function

real(4) function calc4 (r1, nsize)
    real(4), intent(in) :: r1(nsize)
    integer, intent(in) :: nsize

    calc4 = 1.0e0

    do i = 1, nsize
        calc4 = calc4 * r1(i)
    end do
end function
