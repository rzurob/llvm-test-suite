! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/20/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Test the form of read without an io-control-spec
!                               during the dtio.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        real(8), allocatable :: data
        complex(8), pointer :: cx => null()

        contains

        procedure :: readBaseFmtd
        generic :: read(formatted) => readBaseFmtd
    end type

    contains

    subroutine readBaseFmtd (dtv, unit, iotype, v_list, iostat, iomsg)
    use iso_fortran_env
        class(base), intent(inout) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        if (unit /= input_unit) error stop 10_4

        if (.not. allocated(dtv%data)) allocate (dtv%data)

        if (.not. associated(dtv%cx)) allocate (dtv%cx)

        read 10, dtv%data

        read *, dtv%cx

10      format (dp, e25.18)
    end subroutine
end module

program dcmlChildRead011
use m
    class(base), pointer :: b1(:)

    logical(4), external :: precision_r8, precision_x6

    allocate (b1(3))

    read '(dc, DT)', b1(1)

    read (*, '(DT)', decimal='comma') b1(2:3)

    !! verify the results

    if (.not. precision_r8(b1(1)%data, -.5d0*dsin(.5d0))) error stop 1_4

    if (.not. precision_x6(b1(1)%cx, cmplx(-1.5d0*dsin(1.5d0), &
            -1.75d0*dsin(1.75d0), 8))) error stop 2_4

    if (b1(2)%data /= 0.0d0) error stop 3_4

    if (.not. precision_x6(b1(2)%cx, cmplx(2.0d0*dlog(2.0d0), &
            3.0d0*dlog(3.0d0), 8)))  error stop 4_4

    if (.not. precision_r8(b1(3)%data, 4.0d0*dlog(4.0d0))) error stop 5_4

    if (.not. precision_x6(b1(3)%cx, cmplx(5.0d0*dlog(5.0d0), &
            6.0d0*dlog(6.0d0), 8)))  error stop 6_4
end
