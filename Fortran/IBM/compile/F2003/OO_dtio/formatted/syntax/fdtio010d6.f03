! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/18/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : DTIO generics (syntax check for formatted
!                               write)
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

program fdtio010d6
    interface WRITE(FORMATTED)
        subroutine writePointFormatted (dtv, unit, iotype, v_list, iostat, iomsg)
        use m
            class (point), intent(in), value :: dtv
            integer, intent(in), volatile :: unit
            character(*), intent(in), optional :: iotype
            integer, intent(in), target :: v_list(:)
            integer, intent(out), pointer :: iostat
            character(*), intent(inout), save :: iomsg
        end subroutine
    end interface

end
