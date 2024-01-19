
module m
    type dataType
        class(*), allocatable :: x(:)

        contains

        procedure :: writeDataTypeFmtd
        generic :: write(formatted) => writeDataTypeFmtd
    end type

    type base
        class (dataType), allocatable :: data

        contains

        procedure :: writeBaseFmtd
        generic :: write(formatted) => writeBaseFmtd
    end type

    contains

    recursive subroutine writeDataTypeFmtd (dtv, unit, iotype, v_list, iostat, iomsg)
        class(dataType), intent(in) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        if (.not. allocated(dtv%x)) then
            iostat = 1000
            return
        end if

        do i = lbound(dtv%x,1), ubound(dtv%x,1)
            select type (x => dtv%x(i))
                type is (real)
                    write (unit, '(g12.5, 1x)', iostat=iostat, iomsg=iomsg) x

                type is (complex)
                    write (unit, *, round='up', iostat=iostat, iomsg=iomsg) x

                type is (base)
                    write (unit, '(DT, 1x)', iostat=iostat, iomsg=iomsg) x

                class default
                    stop 10
            end select

            if (iostat /= 0) return
        end do
    end subroutine

    recursive subroutine writeBaseFmtd (dtv, unit, iotype, v_list, iostat, iomsg)
        class(base), intent(in) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        if (.not. allocated(dtv%data)) then
            iostat = 2000
            return
        end if

        write(unit, '(DT)', iostat=iostat, iomsg=iomsg) dtv%data
    end subroutine
end module

program dcmlChildWrite005
use m
    type(base), allocatable :: b1(:)

    character(:), allocatable :: c, d

    allocate (b1(12))

    do i = 1, 12, 3
        b1(i) = base(dataType((/(j*1.1, j=1,i)/)))

        b1(i+1) = base(dataType(cmplx((/(j, j=1,i+1)/), (/(2*j, j=1,i+1)/))))

        b1(i+2) = base(dataType((/base(dataType(sin((/(j*1.0, j=1, i+2)/))))/)))

    end do

    open (10, file = 'dcmlChildWrite005.out', decimal='commA')

    write (10, *) b1

    allocate (character(2000) :: c, d)

    write (c, *, decimal='coMMa') b1

    rewind 10

    read (10, '(a)') d

    close(10)

    if (c /= d) error stop 1_4
end
