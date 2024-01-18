!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : funcRetrn002akl
!*
!*  PROGRAMMER                 : David Forster (derived from funcRetrn002a by Robert Ma)
!*  DATE                       : 2007-08-07 (original: 04/26/2005)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : GENERIC BINDING:
!*                                  Cross Feature: Function Return
!*                                    -  function result is a (non-)polymorphic array entity with unformatted i/o
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type base (lbase1) ! lbase1=3
      integer, len :: lbase1
      character(lbase1) :: c = 'xxx'
      contains
         procedure, pass :: write => writeb
         generic :: write(unformatted) => write
   end type

   type, extends(base) :: child (kchild1) ! kchild1=4
      integer, kind :: kchild1
      integer(kchild1) :: i = -999
      contains
         procedure, pass :: write => writec
   end type

   contains

      subroutine writeb (dtv, unit, iostat, iomsg)
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, iostat=iostat, iomsg=iomsg) dtv%c
         iomsg = 'dtiowriteb'

      end subroutine

      subroutine writec (dtv, unit, iostat, iomsg)
         class(child(*,4)), intent(in) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i

         iomsg = 'dtiowritec'

      end subroutine

      class(base(3)) function returnMe(dtv) ! tcx: (3)
         class(base(*)), intent(in) :: dtv(:) ! tcx: (*)
         allocatable :: returnMe(:)

         allocate ( returnMe(size(dtv,1)), source = dtv )

      end function

end module

program funcRetrn002akl
   use m

   integer :: stat
   character(200) :: msg
   character(9) :: cc1
   character(12) :: cc2
   character(3) :: cc3, cc4, cc5
   integer :: i1, i2, i3

   type(base(:)), pointer     :: b1(:) ! tcx: (:)
   class(base(:)), allocatable :: b2(:) ! tcx: (:)
   class(child(:,4)), allocatable, target :: c1(:) ! tcx: (:,4)

   interface
      type(base(3)) function returnMeExt(dtv) ! tcx: (3)
         import base
         type(base(*)), intent(in) :: dtv(3) ! tcx: (*)
         dimension :: returnMeExt(3)
      end function
   end interface

   allocate ( b1(3), source = (/ base(3)('abc'), base(3)('def'), base(3)('ghi') /) ) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   allocate ( b2(4), source = (/ base(3)('ABC'), base(3)('DEF'), base(3)('GHI'), base(3)('JKL') /) ) ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   allocate ( c1(3), source = (/ child(3,4) ( 'abc', 10001 ), child(3,4) ( 'def', 10002 ), child(3,4) ( 'ghi', 10003 ) /) ) ! tcx: (3,4) ! tcx: (3,4) ! tcx: (3,4)

   open ( 1, file = 'funcRetrn002akl.1', form='unformatted', access='sequential' )

   write ( 1, iostat = stat, iomsg = msg )       returnMeExt(b1)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) )        error stop 1_4

   write ( 1, iostat = stat, iomsg = msg )       returnMe(b2)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) )        error stop 2_4

   write ( 1, iostat = stat, iomsg = msg )       returnMe(c1)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) )        error stop 3_4

   rewind 1

   read ( 1, iostat = stat, iomsg = msg )        cc1
   read ( 1, iostat = stat, iomsg = msg )        cc2
   read ( 1, iostat = stat, iomsg = msg )        cc3, i1, cc4, i2, cc5, i3

   if ( ( cc1 /= 'abcdefghi' ) .or. ( cc2 /= 'ABCDEFGHIJKL' ) .or. &
        ( cc3 /= 'abc' ) .or. ( i1 /= 10001 ) .or. &
        ( cc4 /= 'def' ) .or. ( i2 /= 10002 ) .or. &
        ( cc5 /= 'ghi' ) .or. ( i3 /= 10003 ) ) error stop 4_4

   close ( 1, status = 'delete' )

end program

type(base(3)) function returnMeExt(dtv) ! tcx: (3)
  use m, only: base
  type(base(*)), intent(in) :: dtv(3) ! tcx: (*)
  dimension :: returnMeExt(3)
  returnMeExt = dtv
end function


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase1) to invoke with (3) / declare with (*) - 16 changes
! type: child - added parameters (kchild1) to invoke with (3,4) / declare with (*,4) - 5 changes
