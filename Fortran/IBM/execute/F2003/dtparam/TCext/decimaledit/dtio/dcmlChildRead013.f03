! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/F2003/decimaledit/dtio/dcmlChildRead013.f
! opt variations: -qck -qnok -ql

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/26/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Test that the NAN and INF does not affect the
!                               decimal edit mode in DTIO.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1,k2)    ! (4,8)
        integer, kind            :: k1,k2
        integer(k1)                 id
        complex(k2), allocatable :: data
        character(:), allocatable :: name

        contains

        procedure :: readBaseFmtd
        generic :: read(formatted) => readBaseFmtd
    end type

    contains

    subroutine readBaseFmtd (dtv, unit, iotype, v_list, iostat, iomsg)
        class(base(4,8)), intent(inout) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        integer :: stringLen

        read (unit, *, iostat=iostat, iomsg=iomsg) dtv%id

        if (iostat /= 0) return

        if (.not. allocated(dtv%data)) allocate(dtv%data)

        read (unit, *, iostat=iostat, iomsg=iomsg) dtv%data

        if (iostat /= 0) return

        read (unit, *, iostat=iostat, iomsg=iomsg) stringLen

        if (iostat /= 0) return

        dtv%name = repeat(' ', stringLen)

        read (unit, '(a)', iostat=iostat, iomsg=iomsg) dtv%name
    end subroutine
end module

program dcmlChildRead013
use m
use, intrinsic :: ieee_arithmetic

    integer, allocatable :: i1(:)
    complex(8), pointer :: cx1(:)

    logical(4), external :: precision_x6

    type A(k3)    ! (4)
        integer, kind :: k3
        character(:), allocatable :: name
    end type

    class(A(4)), allocatable :: a1(:)

    class(base(4,8)), pointer :: b1(:)

    integer, parameter :: arraySize  = 2*100

    allocate (cx1(arraySize), b1(arraySize), a1(arraySize))

    i1 = (/(j, j=1,arraySize)/)

    cx1 = (/(/(cmplx(sin(j*1.0d0), cos(j*1.0d0),8), j=1,3)/), &
        cmplx(ieee_value(1.0d0, ieee_quiet_nan), &
        ieee_value(1.0d0,ieee_positive_inf), 8), &
        cmplx(ieee_value(1.0d0, ieee_negative_inf), &
        ieee_value(1.0d0, ieee_positive_denormal), 8), &
        (/(cmplx(sin(j*1.0d0), cos(j*1.0d0),8), j=6,arraySize)/)/)

    do i = 1, arraySize
        a1(i)%name = repeat('IBM', i)
    end do

    open (1, file='dcmlChildRead013.data')

    write (1, 999) (i1(i), cx1(i), i*3, a1(i)%name, i = 1, arraySize)

    rewind 1

    read (1, *, decimal='cOMMa') b1

    close (1)

    !! now verify b1
    do i = 1, arraySize
        if (b1(i)%id /= i) error stop 1_4

        if (.not. allocated(b1(i)%name)) error stop 2_4
        if (b1(i)%name /= repeat('IBM', i)) error stop 3_4

        if (.not. allocated(b1(i)%data)) error stop 4_4

        select case(i)
            case (4)
                if ((.not. ieee_is_nan(real(b1(i)%data))) .or. &
                    ieee_is_finite(aimag(b1(i)%data))) error stop 5_4

                if (ieee_is_negative(aimag(b1(i)%data))) error stop 6_4

            case (5)
                if (ieee_is_finite(real(b1(i)%data)) .or. &
                    ieee_is_normal(aimag(b1(i)%data))) error stop 7_4

                if ((.not. ieee_is_negative(real(b1(i)%data))) .or. &
                    ieee_is_negative(aimag(b1(i)%data))) error stop 8_4

            case default
                if (.not. precision_x6(b1(i)%data, &
                    cmplx(sin(i*1.0d0), cos(i*1.0d0),8))) error stop 10_4

        end select
    end do


999 format (dc, i5, 1x, "; (", d25.18, " ; ", d25.18, " ); ", i5, 1x, a)
end
