!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 03/03/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : DTIO generics (ENDFILE statement not allowed in
!                               DTIO)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        complex(8), pointer :: data => null()
    end type

    interface write(unformatted)
        subroutine unformattedWrite (dtv, unit, iostat, iomsg)
        import base
            class (base), intent(in) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
end module

program fdtio509a2
use m
    type (base) :: b1(2)

    integer stat1
    character(200) err

    allocate (b1(1)%data, source=(1.0_8,2.9_8))

    open (1, file='fdtio509a2.data', form='unformatted')

    write (1, iostat=stat1, iomsg=err) b1   !<-- should fail

    if (stat1 /= 0) then
        print *, stat1, err
        error stop 2_4
    end if

    close(1, status='delete')
end

subroutine unformattedWrite (dtv, unit, iostat, iomsg)
use m, only: base
    class (base), intent(in) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (associated (dtv%data)) then
        write (unit, iostat=iostat, iomsg=iomsg) dtv%data

        flush unit
    else
        endfile unit  !<-- illegal
    end if
end subroutine
