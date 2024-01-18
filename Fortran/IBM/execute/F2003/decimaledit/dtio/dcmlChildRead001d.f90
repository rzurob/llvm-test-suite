! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/12/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Test that wrong value separator used as in
!                               list-directed read or namelist read will result
!                               in failures in child READ statement.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        complex, allocatable :: data

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

        if (.not. allocated(dtv%data)) &
            allocate (dtv%data, source=cmplx(-1.0,-1.0))

        read (unit, *, iostat=iostat, iomsg=iomsg) dtv%data
    end subroutine
end module

program dcmlChildRead001d
use m
    character(:), allocatable :: c

    class(base), allocatable :: b1(:)

    logical(4), external :: precision_x8

    c = repeat(' ', 100)

    write (c, '("(", e15.8, ";", e15.8, ") (", e15.8, " , ", e15.8, ")")', &
        decimal='comma') (1.20, 2.2), (3.2, 4.2)

    allocate (b1(2))

    !! the following read will fail to read for the 2nd element for b1
    read (c, *, iostat=istat, decimal='comma') b1

    if (istat == 0) error stop 1_4

    if (.not. precision_x8(b1(1)%data, (1.20, 2.2))) error stop 2_4

    if (.not. precision_x8(b1(2)%data, (-1.0, -1.0))) error stop 3_4
end
