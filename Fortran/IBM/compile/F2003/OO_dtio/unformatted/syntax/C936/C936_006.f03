! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/04/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: TYPE(derived-type-spec) in DTIO subroutine
!*                                        shall be invoked for non-extensible type
!*                                          - try define dtv to be intrinsic type
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program C936_006

    interface read(unformatted)
       subroutine unformattedRead1 (dtv, unit, iostat, iomsg)
          integer, intent(inout) :: dtv
          integer, intent(in) :: unit
          integer, intent(out) :: iostat
          character(*), intent(inout) :: iomsg
       end subroutine
    end interface

    interface write(unformatted)
       subroutine unformattedWrite1 (dtv, unit, iostat, iomsg)
          integer, intent(in) :: dtv
          integer, intent(in) :: unit
          integer, intent(out) :: iostat
          character(*), intent(inout) :: iomsg
       end subroutine
    end interface

end program


subroutine unformattedRead (dtv, unit, iostat, iomsg)
    integer, intent(inout) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    integer :: temp
    read (unit, iostat=iostat, iomsg=iomsg ) temp
    dtv = temp + 1

end subroutine


subroutine unformattedWrite (dtv, unit, iostat, iomsg)
    integer, intent(in) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    write (unit, iostat=iostat, iomsg=iomsg ) dtv + 1

end subroutine
