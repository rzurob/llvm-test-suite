! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-09-10 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Finalization process
!*                               Try DTIO inside the finalization process
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
   contains
      procedure, pass :: getC
      procedure, pass :: setC
      final :: finalbase, finalbaserank1
   end type

   interface write(unformatted)
      subroutine writeUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   interface read(unformatted)
      subroutine readUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

contains

   character(3) function getC(dtv)
      class(base(*)), intent(in) :: dtv ! tcx: (*)
      getC = dtv%c
   end function

   subroutine setC(dtv, c)
      class(base(*)), intent(inout) :: dtv ! tcx: (*)
      character(3) :: c
      dtv%c = c
   end subroutine

   subroutine finalbase(dtv)
      type(base(*)), intent(in) :: dtv ! tcx: (*)
      integer :: stat
      character(200) :: msg

      write (unit = 3, iostat=stat, iomsg=msg)   dtv

      if ( (stat /= 0 ) .or. ( msg /= 'dtiowrite') )   error stop 101_4

   end subroutine

   subroutine finalbaserank1(dtv)
      type(base(*)), intent(in) :: dtv(:) ! tcx: (*)
      integer :: stat
      character(200) :: msg

      write (unit = 3, iostat=stat, iomsg=msg)   dtv

      if ( (stat /= 0 ) .or. ( msg /= 'dtiowrite') )   error stop 2_4

   end subroutine

end module


program final001l
   use m1

   ! declaration of variables
   class(base(:)), allocatable :: b1 ! tcx: (:)
   class(base(:)), pointer     :: b2 ! tcx: (:)
   type(base(:)) , allocatable :: b3 ! tcx: (:)
   class(base(:)), pointer     :: b4(:) ! tcx: (:)

   integer :: stat
   character(200) :: msg

   open (unit = 3, file ='final001l.3', form='unformatted', access='stream')

   ! allocation of variables

   allocate (b1, source = base(3)('abc') )           !<- finalized ! tcx: (3)
   allocate (b2, source = base(3)('def') )           !<- finalized ! tcx: (3)
   allocate (b3, source = base(3)('ghi') )           !<- finalized ! tcx: (3)
   allocate (b4(3), source = (/ b3, b2, b1 /) )   !<- no finalization here!

   ! unformatted I/O operations

   read ( 3, iostat = stat, iomsg = msg, pos=1 )   b2
      if ( (stat /= 0 ) .or. ( msg /= 'dtioread') )   error stop 3_4
   read ( 3, iostat = stat, iomsg = msg, pos=4 )   b3
      if ( (stat /= 0 ) .or. ( msg /= 'dtioread') )   error stop 4_4
   read ( 3, iostat = stat, iomsg = msg, pos=7 )   b1
      if ( (stat /= 0 ) .or. ( msg /= 'dtioread') )   error stop 5_4
   read ( 3, iostat = stat, iomsg = msg, pos=1 )   b4
      if ( (stat /= 0 ) .or. ( msg /= 'dtioread') )   error stop 6_4

   if ( b2%getC() /= 'abc' )            error stop 7_4
   if ( b3%getC() /= 'def' )            error stop 8_4
   if ( b1%getC() /= 'ghi' )            error stop 9_4
   if ( ( b4(1)%getC() /= 'abc' ) .or. ( b4(2)%getC() /= 'def' ) .or. &
        ( b4(3)%getC() /= 'ghi' ) )     error stop 10_4

   deallocate (b1)  !<- finalized       write at pos 10
   deallocate (b2)  !<- finalized       write at pos 13
   deallocate (b3)  !<- finalized       write at pos 16
   deallocate (b4)  !<- finalized       write at pos 19 - 27

   allocate (base(3):: b1, b2, b3, b4(3) ) ! tcx: base(3)

   read ( 3, iostat = stat, iomsg = msg, pos=10 )   b4
      if ( (stat /= 0 ) .or. ( msg /= 'dtioread') )   error stop 11_4
   read ( 3, iostat = stat, iomsg = msg, pos=19 )   b1
      if ( (stat /= 0 ) .or. ( msg /= 'dtioread') )   error stop 12_4
   read ( 3, iostat = stat, iomsg = msg, pos=22 )   b2
      if ( (stat /= 0 ) .or. ( msg /= 'dtioread') )   error stop 13_4
   read ( 3, iostat = stat, iomsg = msg, pos=25 )   b3
      if ( (stat /= 0 ) .or. ( msg /= 'dtioread') )   error stop 14_4

   if ( b1%getC() /= 'abc' )            error stop 15_4
   if ( b2%getC() /= 'def' )            error stop 16_4
   if ( b3%getC() /= 'ghi' )            error stop 17_4
   if ( ( b4(1)%getC() /= 'ghi' ) .or. ( b4(2)%getC() /= 'abc' ) .or. &
        ( b4(3)%getC() /= 'def' ) )     error stop 18_4

   ! close the file appropriately

   close ( 3, status ='delete' )

end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1, only: base
   class(base(*)), intent(inout) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   read (unit,  iostat=iostat ) dtv%c

   iomsg = 'dtioread'

end subroutine

subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1,  only: base
   class(base(*)), intent(in) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write (unit, iostat=iostat ) dtv%c

   iomsg = 'dtiowrite'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 15 changes