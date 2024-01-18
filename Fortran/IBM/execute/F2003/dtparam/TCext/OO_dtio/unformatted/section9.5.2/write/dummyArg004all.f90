! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArg004all
!*
!*  DATE                       : 2007-10-03 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer input/output list
!*                               - Try output item to be an non-polymorphic scalar dummy argument with pointer/allocatable attribute
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
   type base (lbase_1) ! lbase_1=3
      integer, len :: lbase_1
      character(lbase_1) :: c = ''
   end type
   type, extends(base) :: child (lchild_1) ! lchild_1=3
      integer, len :: lchild_1
      character(lchild_1) :: cc = ''
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

contains

   subroutine myWriteA(unit, stat, msg, a, b )
      type(base(:)), allocatable, intent(in)  :: a ! tcx: (*)
      type(child(:,:)), allocatable, intent(in) :: b ! tcx: (*,*)
      integer, intent(in)  :: unit
      integer, intent(out) :: stat
      character(*), intent(inout) :: msg

      write(unit, iostat=stat, iomsg=msg) a,b
      if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 101_4

   end subroutine

   subroutine myWriteP(unit, stat, msg, a, b )
      type(base(:)), pointer, intent(in)  :: a ! tcx: (*)
      type(child(:,:)), pointer, intent(in) :: b ! tcx: (*,*)
      integer, intent(in)  :: unit
      integer, intent(out) :: stat
      character(*), intent(inout) :: msg

      write(unit, iostat=stat, iomsg=msg) a,b
      if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 102_4

   end subroutine

end module

program dummyArg004all
   use m1

   ! declaration of variables
   integer :: stat
   character(200) :: msg
   character(9)  :: cc1, cc2

   type(base(:)), pointer :: b1 ! tcx: (:)
   type(base(:)), allocatable :: b2 ! tcx: (:)

   type(child(:,:)), pointer :: c1 ! tcx: (:,:)
   type(child(:,:)), allocatable :: c2 ! tcx: (:,:)

   allocate(base(3):: b1, b2 ) ! tcx: base(3)
   allocate(child(3,3):: c1, c2 ) ! tcx: child(3,3)

   b1%c = 'ABC'
   b2%c = 'DEF'
   c1%c = 'abc'
   c1%cc= 'def'
   c2%c = 'ghi'
   c2%cc= 'jkl'

   open (unit = 1, file ='dummyArg004all.data', form='unformatted', access='sequential')

   ! unformatted I/O operations

   call myWriteP (1, stat, msg, b1, c1 )
   call myWriteA (1, stat, msg, b2, c2 )

   rewind 1

   read (1, iostat=stat, iomsg=msg )              cc1
   read (1, iostat=stat, iomsg=msg )              cc2

   ! check if the values are set correctly

   if ( cc1 /= 'ABCabcdef' )                  error stop 2_4
   if ( cc2 /= 'DEFghijkl' )                  error stop 3_4

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1, only: base, child
   class(base(*)), intent(in) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   select type ( g => dtv )
      type is (base(*)) ! tcx: (*)
         write (unit, iostat=iostat ) g%c
      type is (child(*,*)) ! tcx: (*,*)
         write (unit, iostat=iostat ) g%c, g%cc
   end select
   iomsg = 'dtiowrite'
end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 7 changes
! type: child - added parameters (lchild_1) to invoke with (3,3) / declare with (*,*) - 5 changes
