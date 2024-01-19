! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/19/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Decimal mode on child read on internal file,
!                               where the parent is also on the same internal
!                               file: test that the child read statement on
!                               internal file is in default (POINT) mode.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type dataType
        complex, allocatable :: cx(:)

        contains

        procedure :: readDataTypeFmtd
        generic :: read(formatted) => readDataTypeFmtd
    end type

    type base
        real, pointer :: data => null()
        type(dataType) data2
        real, allocatable :: data3

        contains

        procedure :: readBaseFmtd
        generic :: read(formatted) => readBaseFmtd
    end type

    character(2000) string

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

        if (allocated(dtv%cx)) deallocate (dtv%cx)

        allocate(dtv%cx(arraySize))

        read (unit, *, iostat=iostat, iomsg=iomsg) dtv%cx
    end subroutine

    subroutine readBaseFmtd (dtv, unit, iotype, v_list, iostat, iomsg)
        class(base), intent(inout) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        if (.not. associated(dtv%data)) allocate (dtv%data)

        read (unit, *, iostat=iostat, iomsg=iomsg) dtv%data

        if (iostat /= 0) return

        !! read the dataType from string
        read (string, '(TR15,DT)', iostat=iostat, iomsg=iomsg) dtv%data2

        if (iostat /= 0) return

        if (.not. allocated(dtv%data3)) allocate (dtv%data3)

        read (unit, '(T130, e14.8)', iostat=iostat, iomsg=iomsg) dtv%data3
    end subroutine
end module

program dcmlChildRead009
use m
    class(base), allocatable :: b1(:)

    logical(4), external :: precision_r4, precision_x8

    write (string, &
        '(dc, e14.8,1x,dp, i3,3(", (",e14.8," , ", e14.8, " ) "), dc,e14.8)') &
            1.22, 3, (i*1.11, i*2.11, i=1,3), -1.21

    allocate (b1(10))

    read (string, '(dc, dt)') b1(1)

    if (.not. associated(b1(1)%data)) error stop 1_4

    if (.not. precision_r4(b1(1)%data, 1.22_4)) error stop 2_4

    if (.not. allocated(b1(1)%data3)) error stop 3_4

    if (.not. precision_r4(b1(1)%data3, -1.21_4)) error stop 4_4

    if (.not. allocated(b1(1)%data2%cx)) error stop 5_4

    if (size(b1(1)%data2%cx) /= 3) error stop 6_4

    do i = 1, 3
        if (.not. precision_x8(b1(1)%data2%cx(i), cmplx(i*1.11, i*2.11,4))) &
            error stop 7_4
    end do
end
