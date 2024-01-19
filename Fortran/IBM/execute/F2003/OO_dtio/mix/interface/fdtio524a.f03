
module m
    type base
        real(8), pointer :: r1(:) => null()

        contains

        procedure, non_overridable :: print => printBase
    end type

    type, extends(base) :: child
        complex (4), pointer :: c1(:) => null()
    end type

    interface write(formatted)
        module procedure formattedWrite
    end interface

    contains

    !! this subroutine only accepts list-directed write
    subroutine formattedWrite (dtv, unit, iotype, v_list, iostat, iomsg)
        class (base), intent(in) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        if (iotype /= 'LISTDIRECTED') return

        if (size(v_list) /= 0) error stop 10_4

        if (associated (dtv%r1)) then
            do i = lbound(dtv%r1,1), ubound(dtv%r1,1)
                write (unit, '(f12.2,1x)', iostat=iostat, iomsg=iomsg) dtv%r1(i)

                if (iostat /= 0) return
            end do
        else
            write (unit, '(a)', iostat=iostat, iomsg=iomsg) 'r1 not associated'
        end if

        if (iostat /= 0) return

        select type (dtv)
            type is (child)
                write (unit, '(a,/,1x)', iostat=iostat, iomsg=iomsg) ';'

                if (iostat /= 0) return

                if (associated (dtv%c1)) then
                    do i = lbound(dtv%c1,1), ubound(dtv%c1,1)
                        write (unit, '(a,f12.2,a,f12.2,a,1x)', iostat=iostat, iomsg=iomsg) &
                            '(', real(dtv%c1(i)), ',', aimag(dtv%c1(i)), ')'

                        if (iostat /= 0) return
                    end do
                else
                    write (unit, '(a)', iostat=iostat, iomsg=iomsg) &
                            'c1 not associated'
                end if
            type is (base)
            class default
                error stop 12_4
        end select
    end subroutine

    subroutine printBase (b)
        class(base), intent(in) :: b

        print *, b
    end subroutine
end module

program fdtio524a
use m
    class(base), pointer :: b1
    type (child), target :: c1

    b1 => c1

    call b1%print

    allocate (c1%r1(0:2), source=(/0._8, 1.1_8, 2.2_8/))

    call b1%print

    allocate (c1%c1(2:3), source=(/(2.2_4, -2.2_4), (3.3_4, -3.3_4)/))

    call b1%print
end
