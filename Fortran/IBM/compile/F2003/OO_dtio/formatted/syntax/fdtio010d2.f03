! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/18/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : DTIO generics (syntax check for the formatted
!                               write; reverse the ordering of the dummy-arg)
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
    type point
        real*4 :: x, y
    end type
end module

program fdtio010d2
use m
    interface WRITE(FORMATTED)
        subroutine writePointFormatted (dtv, iotype, unit, v_list, iostat, iomsg)
        use m
            class (point), intent(in) :: dtv
            character(*), intent(in) :: iotype
            integer, intent(in) :: unit
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
end

