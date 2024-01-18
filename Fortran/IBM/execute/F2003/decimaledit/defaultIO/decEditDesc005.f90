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
!*  DATE                       : 06/20/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Test that slashes in format specification have
!                               no effect on the decimal edit mode.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    real(8), allocatable :: r1(:,:)

    type base
        character(:), allocatable :: name
        complex(8) :: cx
        integer(8) :: id
        logical(4) :: flag(10)
    end type

    type(base), allocatable :: b3(:)

    contains

    function atoi (i)
        integer, intent(in) :: i
        character(int(log10(i*1.0)+1)) atoi

        if (len (atoi) < 1) stop 100

        do j = 1, atoi%len
            write (atoi(j:j), '(DC,I1)') (mod(i, 10**(int(atoi%len-j+1))) - &
                    mod(i, int(10**(atoi%len-j)))) / 10**(atoi%len-j)
        end do
    end function
end module

program decEditDesc005
use m
    type(base), allocatable :: b1(:), b2(:)

    real(8) r2(10)

    allocate (b1(10), b3(10))

    b1 = (/(base('xlftest'//atoi(i), i, i, (/(i<j, j=1,10)/)), i = 1, 10)/)

    allocate (r1(2,5), source=reshape((/(i*1.0d1, i=1,10)/), (/2,5/)))

    allocate (b2(10))

    do i = 1, 10
        allocate (character(int(log10(i*1.0)+8)) :: b2(i)%name)
    end do

    b3 = b2

    write (4, 100) r1, (b1(i)%name, b1(i)%cx, b1(i)%id, b1(i)%flag, i=1,10)

    write (4, 200, decimal='Comma') r1, (b1(i)%name, b1(i)%cx, b1(i)%id, &
            b1(i)%flag, i=1,10)


    rewind (4)

    read (4, 100) r2, (b2(i)%name, b2(i)%cx, b2(i)%id, b2(i)%flag, i=1,10)

    call verifyR2B (b2)

    r2 = 1

    read (4, 200, decimal='COMma') r2, (b3(i)%name, b3(i)%cx, b3(i)%id, &
            b3(i)%flag, i=1,10)


    call verifyR2B (b3)

100 format (dc, 10d12.4, /, 10(1x, a, 2d15.7, i10, 10l6))
200 format (10d12.4, /, 10(1x, a, 2d15.7, i10, 10l6), dp)

    contains

    subroutine verifyR2B (b)
        type(base), allocatable, intent(in) :: b(:)

        logical(4), external :: precision_r8, precision_x6

        character(9), parameter :: names(10) = (/'xlftest1 ', 'xlftest2 ', &
            'xlftest3 ', 'xlftest4 ', 'xlftest5 ', 'xlftest6 ', 'xlftest7 ',&
            'xlftest8 ', 'xlftest9 ', 'xlftest10'/)

        do i = 1, 10
            if (.not. precision_r8 (r2(i), i*1.0d1)) error stop 1_4

            if (b(i)%name /= names(i)) error stop 2_4

            if (.not. precision_x6(b(i)%cx, cmplx(i,kind=8))) error stop 3_4

            if (b(i)%id /= i) error stop 4_4

            do j = 1, 10
                if (b(i)%flag(j) .neqv. i<j) error stop 5_4
            end do
        end do
    end subroutine
end
