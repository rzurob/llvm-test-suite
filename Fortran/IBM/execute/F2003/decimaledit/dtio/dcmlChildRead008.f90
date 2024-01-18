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
!*  DATE                       : 07/18/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Test that points (.), commas (,) or semicolons
!                               (;) within a delimited string don't affect read
!                               on other data.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer id
        character(:), allocatable :: name
        complex(8), allocatable :: cx(:)

        contains

        procedure :: readBaseFmtd
        generic :: read(formatted) => readBaseFmtd
    end type

    contains

    subroutine readBaseFmtd (dtv, unit, iotype, v_list, iostat, iomsg)
        class(base), intent(inout) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        character(10000), save :: localBuffer

        integer arraySize

        read (unit, *, iostat=iostat, iomsg=iomsg) dtv%id

        if (iostat /= 0) return

        read (unit, *, iostat=iostat, iomsg=iomsg) localBuffer

        if (iostat /= 0) return

        dtv%name = trim(localBuffer)

        read (unit, *, iostat=iostat, iomsg=iomsg) arraySize

        if (iostat /= 0) return

        dtv%cx = (/(0, i=1, arraySize)/)

        read (unit, *, iostat=iostat, iomsg=iomsg) dtv%cx
    end subroutine
end module

program dcmlChildRead008
use m
    logical(4), external :: precision_x6

    type temp
        character(:), allocatable :: string
    end type

    type (temp) tmp(10)

    type(base), allocatable :: b1(:)

    tmp(1)%string = 'this line contains comma (,)'
    tmp(2)%string = 'point ( . )'
    tmp(3)%string = 'semicolon (;)'
    tmp(4)%string = ', ; .'
    tmp(5)%string = ' 1,23 in comma mode'
    tmp(6)%string = 'is same as 1.23 in point mode ( . )'
    tmp(7)%string = ''
    tmp(8)%string = 'xlftest 123 ;'
    tmp(9)%string = ' (1.23,2.11)'
    tmp(10)%string = ' (1,23;2,11)'


    open (1, file='dcmlChildRead008.data', delim='quote')

    write (1, *, decimal='comma') (i, tmp(i)%string, i, &
            (cmplx(j,2*j,8), j=1,i), i=1,10)

    rewind 1

    allocate (b1(10))

    read (1, '(dc, 10DT)') b1

    !! verify b1
    do i = 1, 10
        if (b1(i)%id /= i) error stop 1_4

        if ((.not. allocated(b1(i)%name)) .or. &
                (len(b1(i)%name) /= len(tmp(i)%string))) error stop 2_4

        if (b1(i)%name /= tmp(i)%string) error stop 3_4

        if (.not. allocated(b1(i)%cx)) error stop 4_4

        if (size(b1(i)%cx) /= i) error stop 5_4

        do j = 1, i
            if (.not. precision_x6 (b1(i)%cx(j), cmplx(j, 2*j, kind=8))) &
                    error stop 6_4
        end do
    end do
end
