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
!*                               - Try output item to be an non-polymorphic array dummy argument
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

   subroutine myWrite(unit, stat, msg, a, b )
      type(base(*)), intent(in), pointer  :: a(:) ! tcx: (*)
      type(child(*,*)), intent(in), pointer :: b(:) ! tcx: (*,*)
      integer, intent(in)  :: unit
      integer, intent(out) :: stat
      character(*), intent(inout) :: msg

      write(unit, iostat=stat, iomsg=msg) a,b
      if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 101_4

   end subroutine

   subroutine myWrite1(unit, stat, msg, a, b )
      type(base(*)), intent(in), allocatable  :: a(:,:) ! tcx: (*)
      type(child(*,*)), intent(in), allocatable :: b(:,:,:) ! tcx: (*,*)
      integer, intent(in)  :: unit
      integer, intent(out) :: stat
      character(*), intent(inout) :: msg

      write(unit, iostat=stat, iomsg=msg) a, b
      if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 102_4

   end subroutine

end module

program dummyArg005all
   use m1

   ! declaration of variables
   integer :: stat
   character(200) :: msg
   character(30)  :: cc1
   character(60)  :: cc2

   type(base(3))  :: b1(2) = (/ base(3)('ABC'), base(3)('DEF') /) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   type(base(3)), pointer :: b2(:) ! tcx: (:)
   type(base(3)), allocatable :: b3(:,:) ! tcx: (:)

   type(child(3,3)) :: c1(2) = (/ child(3,3)('abc','def'), child(3,3)('ghi','jkl') /) ! tcx: (3,3) ! tcx: (3,3) ! tcx: (3,3)
   type(child(3,3)), pointer :: c2(:) ! tcx: (:,:)
   type(child(3,3)), allocatable :: c3(:,:,:) ! tcx: (:,:)

   allocate( b2(2) , source = b1 )
   allocate( b3(2,2), source = reshape ( source = (/ b1, b1 /) , shape = (/2,2/) ) )
   allocate( c2(4), source = reshape ( source = (/ c1, child(3,3)('mno','pqr'), child(3,3)('stu','vwx') /), shape = (/4/) ) ) ! tcx: (3,3) ! tcx: (3,3)
   allocate( c3(2,2,2), source = reshape ( source = (/ c2, c2 /), shape = (/2,2,2/) ) )

   open (unit = 1, file ='dummyArg005all.data', form='unformatted', access='sequential')

   ! unformatted I/O operations

   call myWrite (1, stat, msg, b2, c2 )
   call myWrite1 (1, stat, msg, b3, c3 )

   rewind 1

   read (1, iostat=stat, iomsg=msg )              cc1
   read (1, iostat=stat, iomsg=msg )              cc2

   ! check if the values are set correctly

   if ( cc1 /= 'ABCDEFabcdefghijklmnopqrstuvwx' )                                   error stop 2_4
   if ( cc2 /= 'ABCDEFABCDEFabcdefghijklmnopqrstuvwxabcdefghijklmnopqrstuvwx' )     error stop 3_4

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
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 10 changes
! type: child - added parameters (lchild_1) to invoke with (3,3) / declare with (*,*) - 10 changes
