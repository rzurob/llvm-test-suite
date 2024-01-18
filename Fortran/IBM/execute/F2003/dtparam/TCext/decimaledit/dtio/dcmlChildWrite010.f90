! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=self /tstdev/F2003/decimaledit/dtio/dcmlChildWrite010.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=none

! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/12/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Test the value separator (, or ;) for the child
!                               write statement under different decimal edit
!                               mode; use namelist and list-directed write for
!                               child.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A(n1,k1)    ! (20,4)
        integer, kind            :: k1
        integer, len             :: n1
        complex(k1), allocatable :: cx

        contains

        procedure :: writeAFmtd
        generic :: write(formatted) => writeAFmtd
    end type

    type B(n2,k2)    ! (20,8)
        integer, kind        :: k2
        integer, len         :: n2
        complex(k2), pointer :: cx

        contains

        procedure :: writeBFmtd
        generic :: write(formatted) => writeBFmtd
    end type

    contains

    subroutine writeAFmtd (dtv, unit, iotype, v_list, iostat, iomsg)
        class(A(*,4)), intent(in) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        character decMode

        if (allocated(dtv%cx)) then
            inquire (unit, decimal=decMode, iostat=iostat, iomsg=iomsg)

            if (iostat /= 0) return

            if (decMode == 'P') then
                write(unit, *, sign='plus', decimal='Comma', iostat=iostat, &
                        iomsg=iomsg) dtv%cx
            else if (decMode == 'C') then
                write(unit, *, sign='plus', decimal='POINt', iostat=iostat, &
                        iomsg=iomsg) dtv%cx
            else
                error stop 11_4
            end if
        else
            error stop 10_4
        end if
    end subroutine

    subroutine writeBFmtd (dtv, unit, iotype, v_list, iostat, iomsg)
        class(B(*,8)), intent(in) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        type x(n3,k3)    ! (20,8)
            integer, kind :: k3
            integer, len  :: n3
            complex(k3)      cx
        end type

        type(x(20,8)) :: b1

        namelist /base/ b1

        character decMode

        if (.not. associated(dtv%cx)) error stop 20_4

        b1%cx = dtv%cx

        inquire (unit, decimal=decMode, iostat=iostat, iomsg=iomsg)

        if (iostat /= 0) return

        if (decMode == 'P') then
            write (unit, base, decimal='COmma', iostat=iostat, iomsg=iomsg)
        else if (decMode == 'C') then
            write (unit, base, decimal='PoInT', iostat=iostat, iomsg=iomsg)
        else
            error stop 21_4
        end if
    end subroutine
end module

module n
use m
    type base(k4,n4,k5)    ! (4,20,8)
        integer, kind  :: k4,k5
        integer, len   :: n4
        type(A(n4,k4)) :: a1
        type(B(n4,k5)) :: b1

        contains

        procedure :: writeBaseFmtd
        generic :: write(formatted) => writeBaseFmtd
    end type

    contains

    subroutine writeBaseFmtd (dtv, unit, iotype, v_list, iostat, iomsg)
        class(base(4,*,8)), intent(in) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        character decMode

        inquire (unit, decimal=decMode, iostat=iostat, iomsg=iomsg)

        if (iostat /= 0) return

        if (decMode == 'P') then        ! point mode
            write (unit, '(dc, dt, dp, /, dt)', iostat=iostat, iomsg=iomsg) &
                dtv%a1, dtv%b1
        else if (decMode == 'C') then   ! comma mode
            write (unit, '(dt, dc, /, dt)', iostat=iostat, iomsg=iomsg, &
                decimal='poInt') dtv%a1, dtv%b1
        else                            ! something is wrong
            error stop 30_4
        end if
    end subroutine
end module

program dcmlChildWrite010
use n
    class (base(4,:,8)), allocatable :: b1

    complex(8), target :: cx1

    allocate (b1, source=base(4,20,8) (A(20,4)((1.0, 2.0)), B(20,8)(cx1)))

    cx1 = (3.0d0, 4.0d0)

    open (10, file='dcmlChildWrite010.data', decimal='Comma')

    write (10, '(ss, DT)') b1

    write (10, '(dp,sp, DT)') b1

end
