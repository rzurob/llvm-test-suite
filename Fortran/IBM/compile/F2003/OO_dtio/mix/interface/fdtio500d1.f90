!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/02/2005
!*
!*  DESCRIPTION                : DTIO generics (diagnostic test case for
!                               resolving DTIO routine: unlimited poly can
!                               appear in IO without casting in select type)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer(4), pointer :: i
    end type

    integer(4), parameter :: ISNULL = -999999
end module


program fdtio500d1
use m

    interface write(unformatted)
        subroutine unformattedWrite (dtv, unit, iostat, iomsg)
        use m
            class (base), intent(in) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    class(*), allocatable :: b1
    integer stat
    character(200) :: errormsg

    integer(4), target :: i1 = 200

    allocate (b1, source=base(i=null()))

    write (1) b1   !<-- illegal

    !! the following select type is how to resolve to DTIO
    select type (b1)
        class is (base)
            write (1) b1
        class default
            error stop 10_4
    end select
end


subroutine unformattedWrite (dtv, unit, iostat, iomsg)
use m
    class (base), intent(in) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (associated (dtv%i)) then
        write (unit, iostat=iostat, iomsg=iomsg) dtv%i
    else
        write (unit, iostat=iostat, iomsg=iomsg) ISNULL
    end if
end subroutine

