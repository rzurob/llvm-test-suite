! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/18/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Test that the decimal edit mode is passed to the
!                               child's child read statement.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type dataType
        real, allocatable :: data(:)

        contains

        procedure :: readDataTypeFmtd
        generic :: read(formatted) => readDataTypeFmtd
    end type

    type base
        class(dataType), allocatable :: data
        type(dataType), pointer :: data2 => null()

        contains

        procedure :: readBaseFmtd
        generic :: read(formatted) => readBaseFmtd
    end type

    contains

    subroutine readDataTypeFmtd (dtv, unit, iotype, v_list, iostat, iomsg)
        class(dataType), intent(inout) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        integer arraySize

        read (unit, *, iostat=iostat, iomsg=iomsg) arraySize

        if (iostat /= 0) return

        if (allocated(dtv%data)) deallocate (dtv%data)

        allocate (dtv%data(arraySize))

        do i = 1, arraySize
            read (unit, *, iostat=iostat, iomsg=iomsg) dtv%data(i)

            if (iostat /= 0) return
        end do
    end subroutine


    !! dtv%data is reverse in decimal edit mode
    subroutine readBaseFmtd (dtv, unit, iotype, v_list, iostat, iomsg)
        class(base), intent(inout) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        character mode

        if (allocated(dtv%data)) deallocate (dtv%data)

        allocate (dtv%data)

        inquire (unit, decimal=mode)

        if (mode == 'P') then
            read (unit, '(DT)', iostat=iostat, iomsg=iomsg, decimal='comma') &
                    dtv%data
        else
            read (unit, '(dp, DT)', iostat=iostat, iomsg=iomsg) dtv%data
        end if

        if (iostat /= 0) return

        if (associated(dtv%data2)) deallocate (dtv%data2)

        allocate (dtv%data2)

        read (unit, *, iostat=iostat, iomsg=iomsg) dtv%data2
    end subroutine
end module

program dcmlChildRead007
use m
    class(base), allocatable :: b1(:)

    character(:), allocatable :: fmt

    logical(4), external :: precision_r4

    allocate (b1(10))

    open (1, file='dcmlChildRead007.data')


    allocate(character(50) :: fmt)

    !! test 1, parent read in comma mode
    do i = 1, 10
        write (fmt, '("(i5,1x,", i5, "e15.8, dc,i5,1x,", i5, "e15.8)")') i, i

        write (1, fmt) i, (j*1.1, j=1,i), i, (j*1.1, j=1,i)
    end do

    rewind 1

    read (1, '(dc, 10DT)') b1

    !! verify b1
    do i = 1, 10
        if ((.not. allocated(b1(i)%data)) .or. (.not. associated(b1(i)%data2)))&
            error stop 1_4


        do j = 1, i
            if (.not. precision_r4(b1(i)%data%data(j), j*1.1_4)) error stop 2_4

            if (.not. precision_r4(b1(i)%data2%data(j), j*1.1_4)) error stop 3_4
        end do
    end do


    !! test 2, parent read in point mode
    rewind 1

    do i = 1, 10
        write (fmt, '("(dc, i5,1x,", i5, "e15.8, dp,i5,1x,", i5, "e15.8)")') i, i

        write (1, fmt) i, (j*2.2, j=1,i), i, (j*2.2, j=1,i)
    end do

    rewind 1

    read (1, *) b1

    !! verify b1
    do i = 1, 10
        if ((.not. allocated(b1(i)%data)) .or. (.not. associated(b1(i)%data2)))&
            error stop 5_4


        do j = 1, i
            if (.not. precision_r4(b1(i)%data%data(j), j*2.2_4)) error stop 6_4

            if (.not. precision_r4(b1(i)%data2%data(j), j*2.2_4)) error stop 7_4
        end do
    end do

end