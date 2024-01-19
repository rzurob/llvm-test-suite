! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing:  DTIO
!*                                         TYPE(derived-type-spec) shall not specify an abstract type
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================

module m1
   type, abstract :: base
      integer i
   end type

end module

program dtio001
   use m1
   interface read(unformatted)

        subroutine unformattedRead (dtv, unit, iostat, iomsg)
        import base
            type(base), intent(inout) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine

    end interface

end program

subroutine unformattedRead (dtv, unit, iostat, iomsg)
use m
    type(base), intent(inout) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    integer(4) :: temp
    read(unit)  temp
    dtv%i = temp

end subroutine