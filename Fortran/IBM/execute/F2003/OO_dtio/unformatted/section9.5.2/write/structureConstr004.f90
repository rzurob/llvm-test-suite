! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 9.5.2 (Data Transfer input/output list)
!*                               - output item is an structure constructor of sequence type
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
      sequence
      character(3) :: c1 = ''
   end type

   interface write(unformatted)
      subroutine writeUnformatted (dtv, unit, iostat, iomsg)
         import base
         type(base), intent(in) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module


program structureConstr004
   use m1

   ! declaration of variables

   integer :: stat
   character(200) :: msg
   character(7) :: c1

   ! allocation of variables

   open (unit = 1, file ='structureConstr004.data', form='unformatted', access='sequential')

   ! I/O operations

   write (1, iostat=stat, iomsg=msg )      base('ibm'), 'eor'   !<- this shall only write c1 and 'eor' to file

   rewind 1

   read (1, iostat=stat, iomsg=msg ) c1

   ! check if the values are set correctly

   if ( c1 /= 'ibmZeor' ) error stop 1_4

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1, only: base
   type(base), intent(in) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write (unit, iostat=iostat, iomsg=iomsg ) dtv%c1
   ! add a mark at the end of record, so we know DTIO is used.
   write (unit, iostat=iostat, iomsg=iomsg ) "Z"

end subroutine
