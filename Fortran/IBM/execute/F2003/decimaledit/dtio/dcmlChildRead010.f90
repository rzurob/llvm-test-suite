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
!*  DATE                       : 07/19/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Read on a sequence type in decimal mode of
!                               comma.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    interface read(formatted)
        module procedure readBaseFmtd
    end interface

    contains

    subroutine readBaseFmtd (dtv, unit, iotype, v_list, iostat, iomsg)
        type base
            sequence

            integer id
            double precision, pointer :: data(:) => null()
            complex, allocatable :: cx
        end type

        type(base), intent(inout) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        integer arraySize

        read (unit, *, iostat=iostat, iomsg=iomsg) dtv%id, arraySize

        if (iostat /= 0) return

        if (associated(dtv%data)) deallocate (dtv%data)

        allocate(dtv%data(arraySize))

        read (unit, *, iostat=iostat, iomsg=iomsg) dtv%data

        if (iostat /= 0) return

        if (.not. allocated(dtv%cx)) allocate (dtv%cx)

        read (unit, *, iostat=iostat, iomsg=iomsg) dtv%cx
    end subroutine
end module


program dcmlChildRead010
use m
    logical(4), external :: precision_r8, precision_x8

    type base
        sequence

        integer id
        double precision, pointer :: data(:) => null()
        complex, allocatable :: cx
    end type

    type(base), allocatable :: b1(:)

    character(:), allocatable :: string

    string = repeat(' ', 2000)

    write (string, *, delim='quote', decimal='comma') (100-i, i, &
            (j*1.2d0, j=1,i), cmplx(i, 100-i), i=1, 12)


    allocate (b1(0:11))

    read (string, '(12DT)', decimal='comma') b1

    !! verify b1
    do i = 1, 12
        if (b1(i-1)%id /= 100-i) error stop 1_4

        if ((.not. associated(b1(i-1)%data)) .or. (size(b1(i-1)%data) /= i)) &
                error stop 2_4

        do j = 1, i
            if (.not. precision_r8(b1(i-1)%data(j), j*1.2d0)) error stop 3_4
        end do

        if (.not. precision_x8(b1(i-1)%cx, cmplx(i, 100-i, 4))) error stop 4_4
    end do
end
