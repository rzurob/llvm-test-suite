! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/17/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Test that change of decimal mode during one
!                               particular child read will NOT affect other read
!                               statements.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer id
        real(4), allocatable :: data
        complex(4) :: cx

        contains

        procedure :: readBaseFmtd
        generic :: read(formatted) => readBaseFmtd
    end type

    contains

    !! the allocatable component is written in reverse decimal mode
    subroutine readBaseFmtd (dtv, unit, iotype, v_list, iostat, iomsg)
        class(base), intent(inout) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        character(5) fMode

        read (unit, *, iostat=iostat, iomsg=iomsg) dtv%id

        if (iostat /= 0) return

        inquire (unit, decimal=fMode)

        if (fMode == 'COMMA') then
            fMode = 'POINT'
        else if (fMode == 'POINT') then
            fMode = 'COMMA'
        else
            iostat = 800
            iomsg = 'decimal mode not known!'

            return
        end if

        if (.not. allocated(dtv%data)) allocate(dtv%data)

        read (unit, *, iostat=iostat, iomsg=iomsg, decimal=fMode) dtv%data

        if (iostat /= 0) return

        read (unit, *, iostat=iostat, iomsg=iomsg) dtv%cx
    end subroutine
end module

program dcmlChildRead005
use m
    class(base), pointer :: b1(:)

    character(:), allocatable :: fmt, separators(:,:)

    logical(4), external :: precision_r4, precision_x8

    allocate (character(4) :: separators(3,2))

    separators (:,1) = (/'" ("', '" ,"', '" )"'/)

    separators (:,2) = (/'" ("', '" ;"', '" )"'/)

    allocate (character(50) :: fmt)

    open (1, file = 'dcmlChildRead005.data', decimal='Comma')

    fmt = '(dp, i5,1x, e15.8, ' // separators(1,2) // ', dc, e15.8,' // &
            separators(2,2) // ', e15.8,' // separators(3,2) // ')'

    write (1, fmt) (i, i*1.2e0, cmplx(i*2.3, (i*4.3)**2), i=1,10)

    fmt = '(10(i5,1x, e15.8, ' // separators(1,1) // ', dp, e15.8,' // &
            separators(2,1) // ', e15.8,' // separators(3,1) // ', dc))'

    write (1, fmt) (i, i*1.2e0, cmplx(i*2.3, (i*4.3)**2), i=11,20)

    rewind 1

    allocate (b1(20))

    do i = 8, 16
        allocate (b1(i)%data, source=-1.0)
    end do

    open (1, decimal='Point')

    read (1, *, decimal='Comma') b1(::2)

    read (1, '(dp, 10DT)') b1(2::2)

    !! verify the results
    do i = 1, 10
        if (b1(2*i-1)%id  /= i) error stop 1_4

        if (b1(2*i)%id  /= i+10) error stop 2_4

        if ((.not. allocated(b1(2*i-1)%data)) .or. &
            (.not.  allocated(b1(2*i-1)%data))) error stop 3_4


        if (.not. precision_r4(b1(2*i-1)%data, i*1.2e0_4)) error stop 4_4

        if (.not. precision_r4(b1(2*i)%data, (i+10)*1.2e0_4)) error stop 5_4

        if (.not. precision_x8(b1(i*2-1)%cx, cmplx(i*2.3, (i*4.3)**2, 4))) &
                error stop 6_4

        if (.not. precision_x8(b1(i*2)%cx, cmplx((i+10)*2.3, ((i+10)*4.3)**2, &
                4))) error stop 7_4
    end do
end
