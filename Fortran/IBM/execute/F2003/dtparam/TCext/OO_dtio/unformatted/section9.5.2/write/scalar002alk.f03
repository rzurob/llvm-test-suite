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
!*                               - Try output item to be scalar object with unlimited polymorphic array component
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
   type data (ldata_1) ! ldata_1=3
      integer, len :: ldata_1
      character(ldata_1) :: c
   end type

   type base (kbase) ! kbase=1
      integer, kind :: kbase
      class(*), pointer :: d(:) => null()
   end type

end module


program scalar002alk
   use m1

   interface write(unformatted)
      subroutine writeUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base(1)), intent(in) :: dtv ! tcx: (1)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   ! declaration of variables
   class(base(1)), allocatable     :: b1 ! tcx: (1)
   class(base(1)), pointer         :: b2 ! tcx: (1)
   type(base(1)), allocatable      :: b3 ! tcx: (1)
   type(base(1)),  pointer         :: b4 ! tcx: (1)

   character(9) :: c1
   integer(4)   :: i2(2)
   character(6) :: c3
   character(3) :: c4
   integer :: stat
   character(200) :: msg

   ! allocation of variables

   allocate ( b1, source = base(1) () ) ! tcx: (1)
   allocate ( b2, source = base(1) () ) ! tcx: (1)
   allocate ( b3, source = base(1) () ) ! tcx: (1)
   allocate ( b4, source = base(1) () ) ! tcx: (1)

   allocate (b1%d(3), source = (/ 'abc', 'def', 'ghi' /) )
   allocate (b2%d(2), source = (/ 1, 2 /) )
   allocate (b3%d(2), source = (/ data(3)('ABC'), data(3)('DEF') /) ) ! tcx: (3) ! tcx: (3)
   allocate (b4%d(1), source = (/ data(3)('GHI')  /) ) ! tcx: (3)

   open (unit = 1, file ='scalar002alk.data', form='unformatted', access='sequential')

   ! unformatted I/O operations

   write (1, iostat=stat, iomsg=msg )             b1
   if (( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 101_4
   write (1, iostat=stat, iomsg=msg )             b2
   if (( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4
   write (1, iostat=stat, iomsg=msg )             b3
   if (( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4
   write (1, iostat=stat, iomsg=msg )             b4
   if (( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 4_4

   rewind 1

   read (1, iostat=stat, iomsg=msg )              c1
   read (1, iostat=stat, iomsg=msg )              i2
   read (1, iostat=stat, iomsg=msg )              c3
   read (1, iostat=stat, iomsg=msg )              c4

   ! check if the values are set correctly

   if ( c1 /= 'abcdefghi' )                      error stop 5_4
   if ( ( i2(1) /= 1 ) .or. ( i2(2) /= 2 ) )     error stop 6_4
   if ( c3 /= 'ABCDEF' )                         error stop 7_4
   if ( c4 /= 'GHI' )                            error stop 8_4

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1
   class(base(1)), intent(in) :: dtv ! tcx: (1)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   interface write(unformatted)
      subroutine writeDataUnformatted (dtv, unit, iostat, iomsg)
         import data
         class(data(*)), intent(in) :: dtv ! tcx: (*)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   select type ( g => dtv%d )
      type is (character(*))
         write (unit, iostat=iostat )     g
      type is (integer)
         write (unit, iostat=iostat )     g
      type is (data(*)) ! tcx: (*)
         write (unit, iostat=iostat, iomsg = iomsg )     g
         if ( (iostat /= 0 ) .or. ( iomsg /= 'datadtio') ) error stop 9_4
   end select

   iomsg = 'dtiowrite'

end subroutine

subroutine writeDataUnformatted (dtv, unit, iostat, iomsg)
   use m1
   class(data(*)), intent(in) :: dtv ! tcx: (*)
   integer,  intent(in) :: unit
   integer,  intent(out) :: iostat
   character(*),  intent(inout) :: iomsg

   write (unit, iostat=iostat )     dtv%c

   iomsg = 'datadtio'

end subroutine


! Extensions to introduce derived type parameters:
! type: data - added parameters (ldata_1) to invoke with (3) / declare with (*) - 6 changes
! type: base - added parameters (kbase) to invoke with (1) / declare with (1) - 10 changes
