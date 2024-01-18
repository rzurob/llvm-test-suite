!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*

!*  TEST CASE NAME             : fdtio524akl
!*  PROGRAMMER                 : David Forster(derived from fdtio524a)
!*  DATE                       : 2007-08-16
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*  DRIVER STANZA              : xlf2003

module m
    type base (kbase_1) ! kbase_1=8
       integer, kind :: kbase_1
        real(kbase_1), pointer :: r1(:) => null()

        contains

        procedure, non_overridable :: print => printBase
    end type

    type, extends(base) :: child (kchild_1) ! kchild_1=4
       integer, kind :: kchild_1
        complex (kchild_1), pointer :: c1(:) => null()
    end type

    interface write(formatted)
        module procedure formattedWrite
    end interface

    contains

    !! this subroutine only accepts list-directed write
    subroutine formattedWrite (dtv, unit, iotype, v_list, iostat, iomsg)
        class (base(8)), intent(in) :: dtv ! tcx: (8)
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
            type is (child(8,4)) ! tcx: (8,4)
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
            type is (base(8)) ! tcx: (8)
            class default
                error stop 12_4
        end select
    end subroutine

    subroutine printBase (b)
        class(base(8)), intent(in) :: b ! tcx: (8)

        print *, b
    end subroutine
end module


program fdtio524akl
use m
    class(base(8)), pointer :: b1 ! tcx: (8)
    type (child(8,4)), target :: c1 ! tcx: (8,4)

    b1 => c1

    call b1%print

    allocate (c1%r1(0:2), source=(/0._8, 1.1_8, 2.2_8/))

    call b1%print

    allocate (c1%c1(2:3), source=(/(2.2_4, -2.2_4), (3.3_4, -3.3_4)/))

    call b1%print
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (8) / declare with (8) - 4 changes
! type: child - added parameters (kchild_1) to invoke with (8,4) / declare with (8,4) - 2 changes
