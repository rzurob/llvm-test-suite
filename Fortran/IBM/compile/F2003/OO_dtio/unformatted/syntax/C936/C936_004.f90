! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/04/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: TYPE(derived-type-spec) in DTIO subroutine
!*                                        shall be ILLEGAL for extensible type
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
    type :: base
        character(3) :: i
    end type
end module

program C936_004
use m

    interface read(unformatted)
        subroutine unformattedRead (dtv, unit, iostat, iomsg)
        use m, newbase => base
            type(newbase), intent(inout) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    interface write(unformatted)
        subroutine unformattedWrite (dtv, unit, iostat, iomsg)
        use m, newbase => base
            type(newbase), intent(in) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

end program


subroutine unformattedRead (dtv, unit, iostat, iomsg)
use m, newbase1 => base
    type(newbase1), intent(inout) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    character(3) :: temp

    read (unit, iostat=iostat, iomsg=iomsg ) temp

    dtv%i = temp


end subroutine


subroutine unformattedWrite (dtv, unit, iostat, iomsg)
use m
    type(base), intent(in) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    write (unit, iostat=iostat, iomsg=iomsg ) dtv%i

end subroutine
