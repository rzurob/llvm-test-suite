! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer input/output list
!*                               - Try output item to be scalar object with array component
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
   type base
      character(3), allocatable :: c(:)
   end type
end module


program scalar002
   use m1

   interface write(unformatted)
      subroutine writeUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(in) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   ! declaration of variables
   class(base), allocatable     :: b1
   class(base), pointer         :: b2
   type(base), allocatable :: b3
   type(base),  pointer :: b4

   integer :: stat
   character(200) :: msg

   character(9) :: c1
   character(6)  :: c2
   character(12)  :: c3
   character(3)  :: c4

   ! allocation of variables

   allocate ( b1, source = base (null()) )
   allocate ( b2, source = base (null()) )
   allocate ( b3, source = base (null()) )
   allocate ( b4, source = base (null()) )

   allocate (b1%c(3), source = (/ 'abc', 'def', 'ghi' /) )
   allocate (b2%c(2), source = (/ 'ABC', 'DEF' /) )
   allocate (b3%c(4), source = (/ 'abc', 'def', 'ghi', 'jkl' /) )
   allocate (b4%c(1), source = (/ 'ABC' /) )

   open (unit = 1, file ='scalar002.data', form='unformatted', access='sequential')

   ! unformatted I/O operations

   write (1, iostat=stat, iomsg=msg )             b1
   if (( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4
   write (1, iostat=stat, iomsg=msg )             b2
   if (( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4
   write (1, iostat=stat, iomsg=msg )             b3
   if (( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4
   write (1, iostat=stat, iomsg=msg )             b4
   if (( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 4_4

   rewind 1

   read (1, iostat=stat, iomsg=msg )              c1
   read (1, iostat=stat, iomsg=msg )              c2
   read (1, iostat=stat, iomsg=msg )              c3
   read (1, iostat=stat, iomsg=msg )              c4

   ! check if the values are set correctly

   if ( c1 /= 'abcdefghi' )        error stop 5_4
   if ( c2 /= 'ABCDEF' )           error stop 6_4
   if ( c3 /= 'abcdefghijkl' )     error stop 7_4
   if ( c4 /= 'ABC' )              error stop 8_4

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1
   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write (unit, iostat=iostat )     ( dtv%c(i), i = 1, size(dtv%c,1) )
   iomsg = 'dtiowrite'

end subroutine
