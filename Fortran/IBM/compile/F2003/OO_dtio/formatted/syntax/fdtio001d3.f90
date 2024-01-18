! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/16/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : DTIO generics (on formatted read, v_list must
!                               be an assumed shape array)
!*
!*  KEYWORD(S)                 :
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
        real(4), allocatable :: d1
    end type
end module

program fdtio001d3
use m
    interface read(formatted)
        subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
        use m
            class (base), intent(inout) :: dtv
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(2:5)  !<-- must be assumed shape
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
end
