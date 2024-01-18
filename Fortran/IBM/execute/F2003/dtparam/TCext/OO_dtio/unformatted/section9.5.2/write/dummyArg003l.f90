! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArg003l
!*
!*  DATE                       : 2007-10-03 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer input/output list
!*                               - Try output item to be an unlimited polymorphic
!*                               Direct Access
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

   subroutine myWrite1(unit, stat, msg, recn, a, b )
      class(*), intent(in) :: a
      class(*), intent(in), optional :: b
      integer, intent(in)  :: unit
      integer, intent(out) :: stat
      integer, intent(in) :: recn
      character(*), intent(inout) :: msg

      if ( present(b) ) then
         select type (b)
            class is (base(*)) ! tcx: (*)
      	       select type (a)
                  class is (base(*)) ! tcx: (*)
                     write(unit, iostat=stat, iomsg=msg, rec=recn) a,b
               end select
      	 end select
      else
      	 select type (a)
            class is (base(*)) ! tcx: (*)
               write(unit, iostat=stat, iomsg=msg, rec=recn)  a
         end select
      end if
      if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 101_4
   end subroutine

end module

program dummyArg003l
   use m1

   ! declaration of variable
   class(base(:)), allocatable :: b1 ! tcx: (:)
   class(base(:)), pointer     :: b2 ! tcx: (:)
   type(base(:)), allocatable  :: b3 ! tcx: (:)
   type(base(3)) :: b4(2:4)                       !<= explicit shape array ! tcx: (3)
   integer :: stat
   character(200) :: msg
   character(6)  :: c1, c4
   character(12) :: c2
   character(3)  :: c3

   ! allocation of variables
   allocate ( b1, source = child(3)('abc', 'def') ) ! tcx: (3)
   allocate ( b2, source = child(3)('ABC', 'DEF') ) ! tcx: (3)
   allocate ( b3, source = base(3)('def') ) ! tcx: (3)
   b4 =(/ base(3)('ABC'), base(3)('DEF') , base(3)('GHI') /) ! tcx: (3) ! tcx: (3) ! tcx: (3)

   open (unit = 1, file ='dummyArg003l.data', form='unformatted', access='direct', recl=15)

   ! unformatted I/O operations

   call myWrite1 (1, stat, msg, 4, b1 )
   call myWrite1 (1, stat, msg, 3, b1, b2 )
   call myWrite1 (1, stat, msg, 2, b3)
   call myWrite1 (1, stat, msg, 1, b4(2), b3 )

   read (1, iostat=stat, iomsg=msg, rec=1 )              c4
   read (1, iostat=stat, iomsg=msg, rec=2 )              c3
   read (1, iostat=stat, iomsg=msg, rec=3 )              c2
   read (1, iostat=stat, iomsg=msg, rec=4 )              c1

   ! check if the values are set correctly

   if ( c1 /= 'abcdef' )              error stop 2_4
   if ( c2 /= 'abcdefABCDEF' )        error stop 3_4
   if ( c3 /= 'def' )                 error stop 4_4
   if ( c4 /= 'ABCdef' )              error stop 5_4

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1, only: base, child
   class(base(*)), intent(in) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   select type (g => dtv)
      type is (base(*)) ! tcx: (*)
         write (unit, iostat=iostat ) g%c
      type is (child(*)) ! tcx: (*)
         write (unit, iostat=iostat ) g%c, g%cc
   end select

   iomsg = 'dtiowrite'
end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 14 changes
! type: child - added parameters () to invoke with (3) / declare with (*) - 3 changes
