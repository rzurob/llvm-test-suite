!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArg002kl
!*
!*  DATE                       : 2007-08-07 (original: 04/26/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : GENERIC BINDING:
!*                                  Cross Feature: Dummy Argument Association
!*                                    - Dummy Argument being polymorphic scalar entity with formatted i/o
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

   type, extends(base) :: child (kc) ! kc=4
      integer, kind :: kc
      integer(kc) :: i = -999
   end type

   interface
      subroutine mywriteext ( dtv )
         import base
         class(base(:)), pointer, intent(in) :: dtv ! tcx: (*)
      end subroutine
   end interface

   interface
      subroutine myreadext ( dtv )
         import base
         class(base(:)), allocatable, intent(inout) :: dtv ! tcx: (*)
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

         select type ( dtv )
            type is ( base(*) ) ! tcx: (*)
               write (unit, *, iostat=iostat, iomsg=iomsg) dtv%c
               iomsg = 'dtiowriteb'
            type is ( child(*,4) ) ! tcx: (*,4)
               write (unit, *, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
               iomsg = 'dtiowritec'
         end select

      end subroutine

      subroutine read (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         select type ( dtv )
            type is ( base(*) ) ! tcx: (*)
               read (unit, *, iostat=iostat, iomsg=iomsg) dtv%c
               iomsg = 'dtioreadb'
            type is ( child(*,4) ) ! tcx: (*,4)
               read (unit, *, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
               iomsg = 'dtioreadc'
         end select

      end subroutine

      subroutine mywrite ( dtv )
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer :: stat
         character(200) :: msg

         write ( 1, *, iostat = stat, iomsg = msg ) dtv
         select type ( dtv )
            type is ( base(*) ) ! tcx: (*)
               if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 1_4
            type is ( child(*,4) ) ! tcx: (*,4)
               if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) ) error stop 2_4
         end select

      end subroutine

      subroutine myread ( dtv )
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer :: stat
         character(200) :: msg

         read ( 1, *, iostat = stat, iomsg = msg) dtv
         select type ( dtv )
            type is ( base(*) ) ! tcx: (*)
               if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) ) error stop 3_4
            type is ( child(*,4) ) ! tcx: (*,4)
               if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) ) error stop 4_4
         end select

      end subroutine

end module

program dummyArg002kl
   use m

   class(base(:)), allocatable :: b1 ! tcx: (:)
   class(base(:)), pointer     :: b2 ! tcx: (:)

   open ( 1, file='dummyArg002kl.1', form='formatted', access='sequential' )

   allocate ( b1, source = base(3)('abc') ) ! tcx: (3)
   allocate ( b2, source = base(3)('def') ) ! tcx: (3)

   call mywrite ( b1 )
   call mywrite ( b2 )
   call mywriteext ( b2 )

   deallocate ( b1, b2 )
   allocate ( b1, source = child(3,4) ( 'ABC', 101 ) ) ! tcx: (3,4)
   allocate ( b2, source = child(3,4) ( 'DEF', 102 ) ) ! tcx: (3,4)

   call mywrite ( b1 )
   call mywrite ( b2 )
   call mywriteext ( b2 )

   rewind 1

   deallocate ( b1, b2 )
   allocate (base(3):: b1, b2 ) ! tcx: base(3)

   call myread  ( b1 )
   call myread  ( b2 )

   if ( ( b1%c /= 'abc' ) .or. ( b2%c /= 'def' )  ) error stop 5_4

   call myreadext ( b1 )

   if ( b1%c /= 'def' ) error stop 6_4

   deallocate ( b1, b2 )
   allocate ( child(3,4) :: b1, b2 ) ! tcx: (3,4)

   call myread  ( b1 )
   call myread  ( b2 )

   select type ( b1 )
      type is ( child(*,4) ) ! tcx: (*,4)
         select type ( b2 )
            type is ( child(*,4) ) ! tcx: (*,4)
               if ( ( b1%c /= 'ABC' ) .or. ( b1%i /= 101 ) .or. ( b2%c /= 'DEF' ) .or. ( b2%i /= 102 )  ) error stop 7_4
         end select
   end select

   call myreadext ( b1 )

   select type ( b1 )
      type is ( child(*,4) ) ! tcx: (*,4)
         if ( ( b1%c /= 'DEF' ) .or. ( b1%i /= 102 )  ) error stop 8_4
   end select

   close ( 1, status ='delete')

end program

subroutine mywriteext ( dtv )
   use m, only: base, child
   class(base(:)), pointer, intent(in) :: dtv ! tcx: (*)
   integer :: stat
   character(200) :: msg

   write ( 1, *, iostat = stat, iomsg = msg ) dtv

   select type ( dtv )
      type is ( base(*) ) ! tcx: (*)
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 9_4
      type is ( child(*,4) ) ! tcx: (*,4)
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) ) error stop 10_4
   end select

end subroutine

subroutine myreadext ( dtv )
   use m, only: base, child
   class(base(:)), allocatable, intent(inout) :: dtv ! tcx: (*)
   integer :: stat
   character(200) :: msg

   read ( 1, *, iostat = stat, iomsg = msg ) dtv

   select type ( dtv )
      type is ( base(*) ) ! tcx: (*)
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) ) error stop 11_4
      type is ( child(*,4) ) ! tcx: (*,4)
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) ) error stop 12_4
   end select

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lb) to invoke with (3) / declare with (*) - 18 changes
! type: child - added parameters (kc) to invoke with (3,4) / declare with (*,4) - 12 changes
