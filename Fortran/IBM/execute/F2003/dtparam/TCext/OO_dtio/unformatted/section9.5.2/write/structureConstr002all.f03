! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-10-03 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 9.5.2 (Data Transfer input/output list)
!*                               - output item is an structure constructor (with type hierarchy, and select type in DTIO)
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

   type :: base (lbase_1) ! lbase_1=3
      integer, len :: lbase_1
      character(lbase_1) :: c1 = ''
   end type

   type, extends(base) :: child (lchild_1) ! lchild_1=4
      integer, len :: lchild_1
      character(lchild_1) :: c2 = ''
   end type

end module


program structureConstr002all
   use m1

   interface write(unformatted)
      subroutine writeUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   ! declaration of variables

   integer :: stat
   character(200) :: msg
   character (8) :: c1
   character (4) :: c2

   ! allocation of variables

   open (unit = 1, file ='structureConstr002all.data', form='unformatted', access='sequential')

   ! I/O operations

   write (1, iostat=stat, iomsg=msg )      child(3,4)('ibm','nice') ! tcx: (3,4)
   write (1, iostat=stat, iomsg=msg )      base(3)('xlf') ! tcx: (3)

   rewind 1

   read (1, iostat=stat, iomsg=msg ) c1
   read (1, iostat=stat, iomsg=msg ) c2

   ! check if the values are set correctly

   if ( c1 /= 'ibmniceZ' ) error stop 101_4
   if ( c2 /= 'xlfZ' )     error stop 2_4

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1

   class(base(*)), intent(in) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   select type (dtv)
      type is (base(*)) ! tcx: (*)
         write (unit, iostat=iostat, iomsg=iomsg) dtv
      type is (child(*,*)) ! tcx: (*,*)
         write (unit, iostat=iostat, iomsg=iomsg) dtv
   end select

   ! add a mark at the end of record, so we know DTIO is used.
   write (unit, iostat=iostat, iomsg=iomsg ) "Z"

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 4 changes
! type: child - added parameters (lchild_1) to invoke with (3,4) / declare with (*,*) - 2 changes
