! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : array004l
!*
!*  DATE                       : 2007-10-03 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer input/output list
!*                               - Try output item to be array of sequence type
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
      character(lbase_1), pointer :: c
   end type
end module


program array004l
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

   type(base(:)), allocatable   :: b3(:) ! tcx: (:)
   type(base(:)),  pointer      :: b4(:) ! tcx: (:)
   type(base(3))                :: b5(5) ! tcx: (3)

   integer :: stat
   character(200) :: msg

   character(6) :: c3
   character(9) :: c4
   character(15):: c5

   ! allocation of variables

   allocate ( b3(2), source = base(3) (null()) ) ! tcx: (3)
   allocate ( b4(3), source = base(3) (null()) ) ! tcx: (3)

   allocate (b3(1)%c, source = 'abc' )
   allocate (b3(2)%c, source = 'def' )
   allocate (b4(1)%c, source = 'ghi' )
   allocate (b4(2)%c, source = 'jkl' )
   allocate (b4(3)%c, source = 'mno' )
   allocate (b5(1)%c, source = 'ABC' )
   allocate (b5(2)%c, source = 'DEF' )
   allocate (b5(3)%c, source = 'GHI' )
   allocate (b5(4)%c, source = 'JKL' )
   allocate (b5(5)%c, source = 'MNO' )

   open (unit = 1, file ='array004l.data', form='unformatted', access='sequential')

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

   if ( c3 /= 'abcdef' )           error stop 4_4
   if ( c4 /= 'ghijklmno' )        error stop 5_4
   if ( c5 /= 'ABCDEFGHIJKLMNO' )  error stop 6_4

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
