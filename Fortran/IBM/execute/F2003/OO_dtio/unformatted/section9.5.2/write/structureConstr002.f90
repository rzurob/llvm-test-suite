!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: structureConstr002.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 9.5.2 (Data Transfer input/output list)
!*                               - output item is an structure constructor (with type hierarchy)
!*                               Sequential Access
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

   type :: base
      character(3) :: c1 = ''
   end type

   type, extends(base) :: child
      character(4) :: c2 = ''
   end type

end module


program structureConstr002
   use m1

   interface write(unformatted)
      subroutine writeUnformatted2 (dtv, unit, iostat, iomsg)
         import child
         class(child), intent(in) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   ! declaration of variables

   integer :: stat
   character(200) :: msg
   character (9) :: c1

   ! allocation of variables

   open (unit = 1, file ='structureConstr002.data', form='unformatted', access='sequential')

   ! I/O operations

   write (1, iostat=stat, iomsg=msg )      child(c1='ibm',c2='xlfo' )

   rewind 1

   read (1, iostat=stat, iomsg=msg ) c1
      if (stat /= 0 ) error stop 1_4

   ! check if the values are set correctly

   if ( c1 /= 'ibmXxlfoZ' ) error stop 2_4

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine writeUnformatted1 (dtv, unit, iostat, iomsg)
use m1
   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write (unit, iostat=iostat, iomsg=iomsg ) dtv%c1

   ! add a mark at the end of record, so we know DTIO is used.
   write (unit, iostat=iostat, iomsg=iomsg ) "X"

end subroutine

subroutine writeUnformatted2 (dtv, unit, iostat, iomsg)
use m1

   interface write(unformatted)
      subroutine writeUnformatted1 (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(in) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   class(child), intent(in) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write (unit, iostat=iostat, iomsg=iomsg ) dtv%base
   write (unit, iostat=iostat, iomsg=iomsg ) dtv%c2

   ! add a mark at the end of record, so we know DTIO is used.
   write (unit, iostat=iostat, iomsg=iomsg ) "Z"

end subroutine