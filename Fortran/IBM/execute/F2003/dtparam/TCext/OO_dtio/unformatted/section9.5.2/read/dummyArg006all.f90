! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArg006all
!*
!*  PROGRAMMER                 : David Forster (derived from dummyArg006a by Robert Ma)
!*  DATE                       : 2007-09-14 (original: 11/08/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf95)
!*
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer input/output list
!*                               - Try input item to be an non-polymorphic scalar dummy argument with pointer/allocatable attribute
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
      character(lbase_1) :: c = 'xxx'
   end type
   type, extends(base) :: child (lchild_1) ! lchild_1=3
      integer, len :: lchild_1
      character(lchild_1) :: cc = 'xxx'
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

   subroutine myReadA(unit, stat, msg, a, b )
      type(base(*)), allocatable, intent(inout)  :: a ! tcx: (*)
      type(child(*,*)), allocatable, intent(inout) :: b ! tcx: (*,*)
      integer, intent(in)  :: unit
      integer, intent(out) :: stat
      character(*), intent(inout) :: msg

      read(unit, iostat=stat, iomsg=msg) a,b
      if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 101_4

   end subroutine

   subroutine myReadP(unit, stat, msg, a, b )
      type(base(*)), pointer, intent(inout)  :: a ! tcx: (*)
      type(child(*,*)), pointer, intent(inout) :: b ! tcx: (*,*)
      integer, intent(in)  :: unit
      integer, intent(out) :: stat
      character(*), intent(inout) :: msg

      read(unit, iostat=stat, iomsg=msg) a,b
      if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4

   end subroutine

end module

program dummyArg006all
   use m1

   ! declaration of variables
   integer :: stat
   character(200) :: msg
   character(9)  :: cc1, cc2

   type(base(3)), pointer :: b1 ! tcx: (:)
   type(base(3)), allocatable :: b2 ! tcx: (:)
   type(child(3,3)), pointer :: c1 ! tcx: (:,:)
   type(child(3,3)), allocatable :: c2 ! tcx: (:,:)

   allocate(base(3):: b1, b2 ) ! tcx: base(3)
   allocate(child(3,3):: c1, c2 ) ! tcx: child(3,3)

   cc1 = 'ABCabcdef'
   cc2 = 'DEFghijkl'

   open (unit = 1, file ='dummyArg006all.data', form='unformatted', access='sequential')

   ! unformatted I/O operations

   write (1, iostat=stat, iomsg=msg )              cc1
   write (1, iostat=stat, iomsg=msg )              cc2

   rewind 1

   call myReadP (1, stat, msg, b1, c1 )
   call myReadA (1, stat, msg, b2, c2 )

   print *, b1
   print *, b2
   print *, c1
   print *, c2

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
      type is (child(*,*)) ! tcx: (*,*)
         read (unit, iostat=iostat ) g%c, g%cc
   end select
   iomsg = 'dtioread'
end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 7 changes
! type: child - added parameters (lchild_1) to invoke with (3,3) / declare with (*,*) - 5 changes
