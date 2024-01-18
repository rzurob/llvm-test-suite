! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : specifier002l
!*
!*  DATE                       : 2007-09-09 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 9.5 Data Transfer Statement
!*                               C909: no specifier shall appear more than once in READ or WRITE
!*                               - multiple specifier in child data transfer statements
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m1

   type base (lbase_1) ! lbase_1=3
      integer, len :: lbase_1
      character(lbase_1) :: c
   end type

end module

program specifier002l
   use m1

   interface read(formatted)
      subroutine readFormatted (dtv, unit, iotype, v_list, iostat, iomsg)
      import base
         class (base(*)), intent(inout) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

   interface write(formatted)
      subroutine writeFormatted (dtv, unit, iotype, v_list, iostat, iomsg)
      import base
         class (base(*)), intent(in) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

   interface read(unformatted)
      subroutine readUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base(*)),  intent(inout) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout):: iomsg
      end subroutine
   end interface

   interface write(unformatted)
      subroutine writeUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1
   class(base(*)), intent(inout) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character, intent(inout) :: iomsg

   read (unit, iostat= iostat, iostat=iostat, iomsg=iomsg ) dtv%c

end subroutine

subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1
   class(base(*)), intent(in) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character, intent(inout) :: iomsg

   write (unit, unit=unit, iostat=iostat, iomsg=iomsg ) dtv%c

end subroutine


subroutine readFormatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m1
   class(base(*)), intent(inout) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)  :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   read (unit, "(A3)", advance='no', advance='no', iostat=iostat, iomsg=iomsg) dtv%c

end subroutine


subroutine writeFormatted (dtv, unit, iotype, v_list,iostat, iomsg)
use m1
   class(base(*)), intent(in) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)  :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write (unit, *, fmt='(A3)', iostat=iostat, iomsg=iomsg) dtv%c


end subroutine

! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 8 changes
