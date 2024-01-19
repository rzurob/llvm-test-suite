!*  ===================================================================
!*
!*  DATE                       : 2007-08-07 (original: 04/26/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : GENERIC BINDING:
!*                                  Cross Feature: Dummy Argument Association
!*                                    - Dummy Argument being non-polymorphic array entity with formatted i/o
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

   type base (lb) ! lb=3
      integer, len :: lb
      character(lb) :: c = 'xxx'
      contains
         procedure, pass :: write
         procedure, pass :: read
         generic :: write(formatted) => write
         generic :: read(formatted)  => read
   end type

   interface
      subroutine mywriteext ( dtv, i , j )
         import base
         type(base(:)), pointer, intent(in) :: dtv(:) ! tcx: (*)
         integer, intent(in) :: i, j
      end subroutine
   end interface

   interface
      subroutine myreadext ( dtv, i , j )
         import base
         type(base(:)), allocatable, intent(inout) :: dtv(:) ! tcx: (*)
         integer, intent(in) :: i, j
      end subroutine
   end interface

   contains

      subroutine write (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         character(30) :: fmt
         write (fmt, *) "(A",v_list(1),")"

         write (unit, fmt, iostat=iostat, iomsg=iomsg) dtv%c
         iomsg = 'dtiowriteb'

      end subroutine

      subroutine read (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         character(30) :: fmt
         write (fmt, *) "(A",v_list(1),")"

         read (unit, fmt , iostat=iostat, iomsg=iomsg) dtv%c
         iomsg = 'dtioreadb'

      end subroutine

      subroutine mywrite ( dtv, i , j )
         type(base(*)), intent(in) :: dtv(3) ! tcx: (*)
         integer, intent(in) :: i, j
         integer :: stat
         character(200) :: msg

         character(30) :: fmt
         write (fmt, *) "(3(DT(",i,")))"

         write ( 1, fmt, iostat = stat, iomsg = msg, rec = j ) dtv
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 1_4

      end subroutine

      subroutine myread ( dtv, i, j )
         type(base(*)), intent(inout) :: dtv(:) ! tcx: (*)
         integer, intent(in) :: i, j
         integer :: stat
         character(200) :: msg

         character(30) :: fmt
         write (fmt, *) "(3(DT(",i,")))"

         read ( 1, fmt, iostat = stat, iomsg = msg, rec = j ) dtv
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) ) error stop 2_4

      end subroutine

end module

program dummyArg003kl
   use m

   type(base(3))              :: b1(3) = (/ base(3)('abc'), base(3)('def'), base(3)('ghi') /)  ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   type(base(:)), allocatable :: b2(:) ! tcx: (:)
   type(base(:)), pointer     :: b3(:) ! tcx: (:)

   open ( 1, file='dummyArg003kl.1', form='formatted', access='direct', recl=20 )

   allocate ( b2(3), source = (/ base(3)('ABC'), base(3)('DEF'), base(3)('GHI') /) ) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   allocate ( b3(3), source = (/ base(3)('jkl'), base(3)('mno'), base(3)('pqr') /) ) ! tcx: (3) ! tcx: (3) ! tcx: (3)

   call mywrite ( b1, 4, 6 )
   call mywrite ( b2, 5, 1 )
   call mywrite ( b3, 6, 5 )

   call mywriteext ( b3, 4, 10 )

   call myread  ( b1, 6, 5 )
   call myread  ( b2, 4, 6 )
   call myread  ( b3, 5, 1 )

   if ( ( b1(1)%c /= 'jkl' ) .or. ( b1(2)%c /= 'mno' ) .or. ( b1(3)%c /= 'pqr' ) .or. &
        ( b2(1)%c /= 'abc' ) .or. ( b2(2)%c /= 'def' ) .or. ( b2(3)%c /= 'ghi' ) .or. &
        ( b3(1)%c /= 'ABC' ) .or. ( b3(2)%c /= 'DEF' ) .or. ( b3(3)%c /= 'GHI' ) )     error stop 3_4

   call myreadext ( b2, 4, 10 )

   if ( ( b2(1)%c /= 'jkl' ) .or. ( b2(2)%c /= 'mno' ) .or. ( b2(3)%c /= 'pqr' ) )     error stop 4_4

   close ( 1, status ='delete')

end program

subroutine mywriteext ( dtv, i , j )
   use m, only: base
   type(base(:)), pointer, intent(in) :: dtv(:) ! tcx: (*)
   integer, intent(in) :: i, j
   integer :: stat
   character(200) :: msg

   character(30) :: fmt
   write (fmt, *) "(3(DT(",i,")))"

   write ( 1, fmt, iostat = stat, iomsg = msg, rec = j ) dtv
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 5_4

end subroutine

subroutine myreadext ( dtv, i, j )
   use m, only: base
   type(base(:)), allocatable, intent(inout) :: dtv(:) ! tcx: (*)
   integer, intent(in) :: i, j
   integer :: stat
   character(200) :: msg

   character(30) :: fmt
   write (fmt, *) "(3(DT(",i,")))"

   read ( 1, fmt, iostat = stat, iomsg = msg, rec = j ) dtv
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) ) error stop 6_4

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lb) to invoke with (3) / declare with (*) - 20 changes
