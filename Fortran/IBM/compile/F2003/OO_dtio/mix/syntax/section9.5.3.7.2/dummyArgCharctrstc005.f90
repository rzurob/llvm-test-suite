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
! %POSTCMD: dcomp dummyArgCharctrstc005.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Characteristics of DTIO interface and procedures
!*                               shall be the same as ones defined in Section 9.5.3.7.2.
!*                               1) Dummy Argument Characteristics
!*                                  -  Intent
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
      character(3) :: c
   end type
end module

program dummyArgCharctrstc005
   use m

   interface read(unformatted)
      subroutine readUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base) :: dtv                          !<- from intent(inout) to omitting intent attribute
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

   interface write(unformatted)
      subroutine writeUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(in) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(out) :: iomsg          !<- from intent(inout) to intent(in)
      end subroutine
   end interface

   interface read(formatted)
      subroutine readFormatted (dtv, unit, iotype, v_list, iostat, iomsg)
         import base
         class(base), intent(inout) :: dtv
         integer, intent(out) :: unit                !<- from intent(in) to intent(out)
         character(*), intent(in) :: iotype
         integer, intent(in) :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

   interface write(formatted)
      subroutine writeFormatted (dtv, unit, iotype, v_list, iostat, iomsg)
         import base
         class(base), intent(in) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in) :: v_list(:)
         integer, intent(inout) :: iostat            !<- from intent(out) to intent(inout)
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

end program
