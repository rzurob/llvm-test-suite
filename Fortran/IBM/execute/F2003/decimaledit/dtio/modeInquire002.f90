!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/24/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Test the INQUIRE statement during child READ
!                               statement returns correct decimal edit mode.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type dataType
        complex, allocatable :: cx
    end type

    type base
        character(20), pointer :: name => null()
        real, allocatable :: data
        class(dataType), allocatable :: d2
    end type

    character(:), allocatable :: modes

    interface read(formatted)
        procedure readBaseFmtd
        procedure readDataTypeFmtd
    end interface


    contains

    subroutine cleanModes
        modes = repeat(' ', 500)
    end subroutine

    integer function currentPos (s)
        character(*), intent(in) :: s

        currentPos = len (trim(s)) + 1
    end function

    subroutine readDataTypeFmtd (dtv, unit, iotype, v_list, iostat, iomsg)
        class(dataType), intent(inout) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        character(:), allocatable :: mode

        allocate (character(5) :: mode)

        inquire (unit, decimal=mode, iostat=iostat, iomsg=iomsg)

        if (iostat /= 0) return

        write (modes(currentPos(modes):), *, decimal=mode, iostat=iostat, &
                iomsg=iomsg) mode

        if (iostat /= 0) return

        if (.not. allocated(dtv%cx)) allocate (dtv%cx)

        read(unit, *, iostat=iostat, iomsg=iomsg) dtv%cx
    end subroutine

    subroutine readBaseFmtd (dtv, unit, iotype, v_list, iostat, iomsg)
        class(base), intent(inout) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        character(5), allocatable :: mode

        allocate (mode)

        inquire(unit, decimal=mode, iostat=iostat, iomsg=iomsg)

        if (iostat /= 0) return

        write (modes(currentPos(modes):), *, decimal=mode, iostat=iostat, &
            iomsg=iomsg) mode

        if (iostat /= 0) return

        if (.not. associated(dtv%name)) allocate (dtv%name)

        read (unit, *, iostat=iostat, iomsg=iomsg) dtv%name

        if (iostat /= 0) return

        if (.not. allocated(dtv%data)) allocate (dtv%data)

        read (unit, *, iostat=iostat, iomsg=iomsg) dtv%data

        if (iostat /= 0) return

        if (.not. allocated(dtv%d2)) allocate(dtv%d2)

        inquire(unit, decimal=mode, iostat=iostat, iomsg=iomsg)

        if (iostat /= 0) return

        write (modes(currentPos(modes):), *, decimal=mode, iostat=iostat, &
            iomsg=iomsg) mode

        if (iostat /= 0) return

        read (unit, *, iostat=iostat, decimal='COMMA', iomsg=iomsg) dtv%d2

        if (iostat /= 0) return

        inquire(unit, decimal=mode, iostat=iostat, iomsg=iomsg)

        if (iostat /= 0) return

        write (modes(currentPos(modes):), *, decimal=mode, iostat=iostat, &
            iomsg=iomsg) mode

        if (iostat /= 0) return
    end subroutine
end module

program modeInquire002
use m
    class(base), allocatable :: b1(:), b2(:)

    logical(4), external :: precision_r4, precision_x8

    open (1, file='modeInquire002.data')

    call writeData (1)

    rewind 1

    allocate (b1(10), b2(10))

    call cleanModes

    read (1,*) b1

    read (1,*, decimal='comma') b2

    !! verify b1 and b2
    do i = 1, 10
        if (.not. associated(b1(i)%name)) error stop 1_4
        if (b1(i)%name /= 'xlftest '//achar(iachar('0') + i-1)) error stop 2_4

        if (.not. allocated(b1(i)%data)) error stop 3_4
        if (.not. precision_r4(b1(i)%data, i*1.2)) error stop 4_4

        if ((.not. allocated(b1(i)%d2)) .or. (.not. allocated(b1(i)%d2%cx))) &
                error stop 5_4

        if (.not. precision_x8(b1(i)%d2%cx, cmplx(sin(i*1.1), cos(i*1.1), 4)))&
                error stop 6_4
    end do

    do i = 11, 20
        if (.not. associated(b2(i-10)%name)) error stop 7_4
        if (b2(i-10)%name /= 'xlftest '//achar(iachar('0') + i-1)) error stop 8_4

        if (.not. allocated(b2(i-10)%data)) error stop 9_4
        if (.not. precision_r4(b2(i-10)%data, i*1.2)) error stop 10_4

        if ((.not. allocated(b2(i-10)%d2)) .or. &
            (.not. allocated(b2(i-10)%d2%cx))) error stop 11_4

        if (.not. precision_x8(b2(i-10)%d2%cx, cmplx(sin(i*1.1), cos(i*1.1), 4)))&
                error stop 12_4
    end do


    !! verify modes
    print *, modes
end


subroutine writeData (unit)
    integer, intent(in) :: unit

    write (unit, &
        fmt='("''", a, "'' ",dp,e15.8, " (", dc, e15.8," ; ",e15.8," ) ")', &
            sign='plus') &
            ('xlftest '//achar(iachar('0') + i-1), i*1.2, &
                cmplx(sin(i*1.1), cos(i*1.1), 4), i=1,10)


    write (unit, &
        fmt='("''", a, "'' ",dc,e15.8, " (", e15.8," ; ",e15.8," ) ")', &
            sign='plus') &
            ('xlftest '//achar(iachar('0') + i-1), i*1.2, &
                cmplx(sin(i*1.1), cos(i*1.1), 4), i=11,20)
end subroutine
