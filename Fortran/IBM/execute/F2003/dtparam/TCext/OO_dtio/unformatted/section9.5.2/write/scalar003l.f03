! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-10-03 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer input/output list
!*                               - Try output item to be scalar of sequence type
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
      sequence
      character(lbase_1), allocatable :: c
   end type
end module


program scalar003l
   use m1

   interface write(unformatted)
      subroutine writeUnformatted (dtv, unit, iostat, iomsg)
         import base
         type(base(*)), intent(in) :: dtv ! tcx: (*)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   ! declaration of variables

   type(base(:)), allocatable   :: b3 ! tcx: (:)
   type(base(:)),  pointer      :: b4 ! tcx: (:)
   type(base(3))                :: b5 ! tcx: (3)

   integer :: stat
   character(200) :: msg

   character(3) :: c3, c4, c5

   ! allocation of variables

   allocate ( b3, source = base(3) (null()) ) ! tcx: (3)
   allocate ( b4, source = base(3) (null()) ) ! tcx: (3)

   allocate (b3%c, source = 'ghi' )
   allocate (b4%c, source = 'jkl' )
   allocate (b5%c, source = 'mno' )

   open (unit = 1, file ='scalar003l.data', form='unformatted', access='sequential')

   ! unformatted I/O operations

   write (1, iostat=stat, iomsg=msg )             b3
   if (( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 101_4
   write (1, iostat=stat, iomsg=msg )             b4
   if (( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4
   write (1, iostat=stat, iomsg=msg )             b5
   if (( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4

   rewind 1

   read (1, iostat=stat, iomsg=msg )              c3
   read (1, iostat=stat, iomsg=msg )              c4
   read (1, iostat=stat, iomsg=msg )              c5

   ! check if the values are set correctly

   if ( c3 /= 'ghi' )     error stop 4_4
   if ( c4 /= 'jkl' )     error stop 5_4
   if ( c5 /= 'mno' )     error stop 6_4

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1
   type(base(*)), intent(in) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write (unit, iostat=iostat )     dtv%c
   iomsg = 'dtiowrite'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 7 changes
