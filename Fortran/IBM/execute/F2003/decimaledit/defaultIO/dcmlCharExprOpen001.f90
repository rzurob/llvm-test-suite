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
!*  DATE                       : 05/23/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : DECIMAL edit mode
!                               Test the decimal= specifier in OPEN statement
!                               that accepts character expression; use the
!                               dummy-argument as the decimal mode.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program dcmlCharExprOpen001
    real(8) d1(10)

    complex(8) cx1(10)

    real(8), allocatable :: d2(:)
    complex(8), allocatable :: cx2(:)

    logical(4) precision_r8, precision_x6

    character(*), parameter :: POINTMODE = 'POINT    '

    character(20) commaMode

    write(commaMode,'(a)') 'COMMA  '

    call random_number (d1)

    cx1 = cmplx(d1, 2.0*d1, kind=8)

    allocate (d2(10), cx2(10))

    call decimalModeScalar ('test1', commaMode, '(1x, d25.18)', d1(1), d2(1))

    if (.not. precision_r8(d1(1), d2(1))) error stop 1_4

    call decimalModeScalar ('test1', POINTMODE, '(g32.20)', d1(2), d2(2))

    if (.not. precision_r8(d1(1), d2(1))) error stop 2_4

    call decimalModeArray (20, 'test2', 'COMMANDER  '(1:5), &
            '(5("(",d25.17,";",E25.17,")"))', cx1, cx2(::2))

    do i = 1, 5
        if (.not. precision_x6(cx1(i), cx2(2*i-1))) error stop 3_4
    end do

    call decimalModeArray (20, 'test2', 'POINT IS TAKEN'(1:6), &
            '(10("(", G25.16, ",", G25.16, ")"))', cx1(6:10), cx2(2::2))


    do i = 6, 10
        if (.not. precision_x6(cx1(i), cx2(2*i-10))) error stop 4_4
    end do

end


subroutine decimalModeScalar (fileName, decimalMode, fmt, d1, d2)
    character(*), intent(in) :: decimalMode, fileName, fmt
    real(8) d1, d2
    intent(in) :: d1
    intent(out) :: d2

    integer :: iunit = 1

    open (iunit, file=fileName, decimal=decimalMode)

    write (iunit, fmt) d1

    backspace(iunit)

    read(iunit, *) d2
end subroutine


subroutine decimalModeArray (iunit, fileName, decimalMode, fmt, d1, d2)
    character(*), intent(in) :: decimalMode, fileName, fmt
    complex(8), intent(in) :: d1(5)
    complex(8), intent(out) :: d2(5)
    integer, intent(in) :: iunit

    open (iunit, file=fileName, access='stream', form='formatted', &
            decimal=decimalMode)

    write (iunit, fmt) d1

    backspace iunit

    read (iunit, *) d2
end subroutine
