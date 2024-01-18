!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: dcomp C936_005.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/04/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: CLASS(derived-type-spec) in DTIO subroutine
!*                                        shall be ILLEGAL for Class(*)
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program C936_005
    interface read(unformatted)
        subroutine unformattedRead (dtv, unit, iostat, iomsg)
            class(*), intent(inout) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    interface write(unformatted)
        subroutine unformattedWrite (dtv, unit, iostat, iomsg)
            class(*), intent(in) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

end program


subroutine unformattedRead (dtv, unit, iostat, iomsg)
    class(*), intent(inout) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
end subroutine

subroutine unformattedWrite (dtv, unit, iostat, iomsg)
    class(*), intent(in) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
end subroutine
