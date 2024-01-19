! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-09-14 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer input/output list
!*                               - Try input item to be an non-polymorphic array dummy argument
!*                               stream access
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
   type, extends(base) :: child
      character(lbase_1) :: cc = ''
   end type

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

   subroutine myread(unit, stat, msg, a, b )
      type(base(*)), intent(inout), pointer  :: a(:) ! tcx: (*)
      type(child(*)), intent(inout), pointer :: b(:) ! tcx: (*)
      integer, intent(in)  :: unit
      integer, intent(out) :: stat
      character(*), intent(inout) :: msg

      read(unit, iostat=stat, iomsg=msg) a,b
      if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 101_4

   end subroutine

   subroutine myread1(unit, stat, msg, a, b )
      type(base(*)), intent(inout), allocatable  :: a(:,:) ! tcx: (*)
      type(child(*)), intent(inout), allocatable :: b(:,:,:) ! tcx: (*)
      integer, intent(in)  :: unit
      integer, intent(out) :: stat
      character(*), intent(inout) :: msg

      read(unit, iostat=stat, iomsg=msg) a, b
      if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4

   end subroutine

end module

program dummyArg005al
   use m1

   ! declaration of variables
   integer :: stat
   character(200) :: msg
   character(30)  :: cc1
   character(60)  :: cc2

   type(base(3))  :: b1(2) = (/ base(3)('xxx'), base(3)('xxx') /) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   type(base(3)), pointer :: b2(:) ! tcx: (:)
   type(base(3)), allocatable :: b3(:,:) ! tcx: (:)

   type(child(3)) :: c1(2) = (/ child(3)('xxx','xxx'), child(3)('xxx','xxx') /) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   type(child(3)), pointer :: c2(:) ! tcx: (:)
   type(child(3)), allocatable :: c3(:,:,:) ! tcx: (:)

   allocate( b2(2) , source = b1 )
   allocate( b3(2,2), source = reshape ( source = (/ b1, b1 /) , shape = (/2,2/) ) )
   allocate( c2(4), source = reshape ( source = (/ c1, child(3)('mno','pqr'), child(3)('stu','vwx') /), shape = (/4/) ) ) ! tcx: (3) ! tcx: (3)
   allocate( c3(2,2,2), source = reshape ( source = (/ c2, c2 /), shape = (/2,2,2/) ) )

   open (unit = 1, file ='dummyArg005al.data', form='unformatted', access='sequential')

   cc1 = 'ABCDEFabcdefghijklmnopqrstuvwx'
   cc2 = 'ABCDEFGHIJKLabcdefghijklmnopqrstuvwxabcdefghijklmnopqrstuvwx'

   write (1, iostat=stat, iomsg=msg )              cc1
   write (1, iostat=stat, iomsg=msg )              cc2

   ! unformatted I/O operations
   rewind 1

   call myread (1, stat, msg, b2, c2 )
   call myread1 (1, stat, msg, b3, c3 )

   print *, b2
   print *, c2
   print *, b3
   print *, c3

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1, only: base, child
   class(base(*)), intent(inout) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   select type ( g => dtv )
      type is (base(*)) ! tcx: (*)
         read (unit, iostat=iostat ) g%c
      type is (child(*)) ! tcx: (*)
         read (unit, iostat=iostat ) g%c, g%cc
   end select
   iomsg = 'dtioread'
end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 10 changes
! type: child - added parameters () to invoke with (3) / declare with (*) - 10 changes
