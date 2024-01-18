!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fdtio010.f
! %VERIFY: fdtio010.data:fdtio010.vf
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: rm -f fdtio010.data
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/18/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                :
!*
!*  KEYWORD(S)                 : DTIO generics (a functional test for formatted
!                               write)
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        complex(4), allocatable :: data
    end type

    interface write(formatted)
        subroutine formattedWrite (dtv, unit, iotype, v_list, iostat, iomsg)
        import base
            class (base), intent(in) :: dtv
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
end module


subroutine formattedWrite (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base
    class (base), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (.not. allocated (dtv%data)) return

    write (unit, 100, iostat=iostat, iomsg=iomsg) real(dtv%data), aimag (dtv%data)

100 format ("(real = ",g10.4,", imag = ",g10.4,")")
end subroutine


program fdtio010
use m
    type (base) :: b1

    allocate (b1%data, source=(1.21, 3.32))

    open (1, file='fdtio010.data')

    write (1, *) base ((10.32, 1.53))

    write (1, *) b1

end
