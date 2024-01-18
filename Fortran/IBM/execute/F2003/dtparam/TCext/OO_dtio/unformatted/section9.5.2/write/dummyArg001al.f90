! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArg001al
!*
!*  PROGRAMMER                 : David Forster (derived from dummyArg001a by Robert Ma)
!*  DATE                       : 2007-10-03 (original: 11/08/2004)
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
!*                               - Try output item to be an scalar dummy argument of pointer/allocatable
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

   type, extends(base) :: child
      character(lbase_1) :: cc = ''
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
      class(base(*)), intent(in), allocatable :: a ! tcx: (*)
      class(base(*)), intent(in), optional, pointer :: b ! tcx: (*)
      integer, intent(in)  :: unit
      integer, intent(out) :: stat
      character(*), intent(inout) :: msg

      if (.not. present(b) ) then
         write(unit, iostat=stat, iomsg=msg) a
      else
      	 write(unit, iostat=stat, iomsg=msg) a,b
      end if

      if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 101_4

   end subroutine

end module

program dummyArg001al
   use m1

   ! declaration of variables
   class(base(3)), allocatable :: b1 ! tcx: (:)
   class(base(3)), pointer     :: b2 ! tcx: (:)

   integer :: stat
   character(200) :: msg
   character(3)  :: c1
   character(9)  :: c2
   character(12) :: c3

   ! allocation of variables
   allocate ( b1, source = child(3)('abc','def') ) ! tcx: (3)
   allocate ( b2, source = base(3)('ghi') ) ! tcx: (3)

   open (unit = 1, file ='dummyArg001al.data', form='unformatted', access='sequential')

   ! unformatted I/O operations

   call myWrite (1, stat, msg, b1 )
   call myWrite (1, stat, msg, b1, b2 )

   allocate ( b2, source = child(3) ('uvw','xyz') ) ! tcx: (3)

   call myWrite (1, stat, msg, b1, b2 )

   rewind 1

   read (1, iostat=stat, iomsg=msg )              c1
   read (1, iostat=stat, iomsg=msg )              c2
   read (1, iostat=stat, iomsg=msg )              c3

   ! check if the values are set correctly

   if ( c1 /= 'abc' )                    error stop 2_4
   if ( c2 /= 'abcdefghi' )              error stop 3_4
   if ( c3 /= 'abcdefuvwxyz' )           error stop 4_4
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
      type is (child(*)) ! tcx: (*)
         write (unit, iostat=iostat ) g%c, g%cc
   end select

   iomsg = 'dtiowrite'
end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 8 changes
! type: child - added parameters () to invoke with (3) / declare with (*) - 3 changes
