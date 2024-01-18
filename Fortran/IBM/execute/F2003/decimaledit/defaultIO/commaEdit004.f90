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
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Test the decimal mode as 'COMMA'  or 'POINT' for
!                               internal IO; both READ and WRITE.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program commaEdit004
    character(800) :: string

    character(6), allocatable :: decimalMode

    complex(4), allocatable :: cx1(:)
    complex(4) cx2(20)

    logical(4) precision_x8

    interface
        subroutine readArrayComplex (s, cx1, mode)
            character(*), intent(in) :: s
            complex(4), intent(out) :: cx1(:)
            character(*), intent(in), optional :: mode
        end subroutine
    end interface

    allocate (cx1(10))

    allocate(decimalMode, source='COMMA')

    cx1 = cmplx(sqrt((/(1.0*i, i=1,10)/)), sin((/(i*1.0, i=1,10)/)), 4)

    call writeString (string, cx1, decimalMode)

    call writeString (string(400:800), cx1, 'POINT')

    call readArrayComplex(string, cx2(1:10), 'COMMA   ')

    call readArrayComplex(string(300:800),cx2(11:20))

    !! verify cx2
    do i = 1, 10
        if (.not. precision_x8(cx2(i), cmplx(sqrt(i*1.0), sin(i*1.0)))) &
            error stop 1_4

        if (.not. precision_x8(cx2(i+10), cx1(i))) error stop 2_4
    end do
end


subroutine writeString (s, cx1, mode)
    character(*), intent(inout) :: s
    character(*), intent(in) :: mode
    complex(4), intent(in) :: cx1(*)

    write (s, *, decimal=mode) cx1(1:10)
end subroutine


subroutine readArrayComplex (s, cx1, mode)
    character(*), intent(in) :: s
    complex(4), intent(out) :: cx1(:)
    character(*), intent(in), optional :: mode

    if (present(mode)) then
        read (s, *, decimal=mode) cx1
    else
        read (s, *) cx1
    end if
end subroutine
