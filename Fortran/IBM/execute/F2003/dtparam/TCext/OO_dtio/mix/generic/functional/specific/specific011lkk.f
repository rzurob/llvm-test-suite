!*  ===================================================================
!*
!*  TEST CASE NAME             : specific011lkk
!*
!*  DATE                       : 2007-08-09 (original: 04/26/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Section 4.5.4 Type Bound Procedures (generic-binding)
!*                                 - Specific Binding
!*                                    - deferred specific type bound procedure
!*                                         - deferred binding in parent type, and child
!*                                           type has non-overridable type bound in module/external procedure
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

   type, abstract :: base (lbase_1) ! lbase_1=3
      integer, len :: lbase_1
      character(lbase_1) :: c = 'xxx'
      contains
         procedure(wbinf), deferred, pass :: write
         procedure(rbinf), deferred, pass :: read
         generic :: write(unformatted) => write
         generic :: read(unformatted)  => read
   end type

   type, extends(base) :: child (kchild_1) ! kchild_1=4
      integer, kind :: kchild_1
      integer(kchild_1) :: i = -999
      contains
         procedure, non_overridable, pass :: write
         procedure, non_overridable, pass :: read
   end type

   type, extends(child) :: gen3 (kgen3_1) ! kgen3_1=4
      integer, kind :: kgen3_1
      integer(kgen3_1) :: j = -999
   end type

   interface
      subroutine wbinf (dtv, unit, iostat, iomsg)
         import base
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

   interface
      subroutine rbinf (dtv, unit, iostat, iomsg)
         import base
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

   interface
      subroutine write (dtv, unit, iostat, iomsg)
         import child
         class(child(*,4)), intent(in) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

   contains

      subroutine read (dtv, unit, iostat, iomsg)
         class(child(*,4)), intent(inout) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         select type ( dtv )
            type is ( child(*,4) ) ! tcx: (*,4)
               read (unit, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
            type is ( gen3(*,4,4) ) ! tcx: (*,4,4)
               read (unit, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i, dtv%j
         end select

         iomsg = 'dtioreadc'

      end subroutine

end module

program specific011lkk
   use m

   integer(4) :: stat
   character(200) :: msg

   class(base(:)), allocatable  :: b1, b2 ! tcx: (:)
   class(child(:,4)), pointer     :: c1, c2 ! tcx: (:,4)
   class(gen3(:,4,4)), allocatable  :: g1 ! tcx: (:,4,4)

   allocate ( b1, source = child(3,4) ( 'abc', 1001 ) ) ! tcx: (3,4)
   allocate ( b2, source = gen3(3,4,4) ( 'def', 1002, 1003 ) ) ! tcx: (3,4,4)
   allocate ( c1, source = child(3,4) ( 'ghi', 1004 ) ) ! tcx: (3,4)
   allocate ( c2, source = gen3(3,4,4) ( 'jkl', 1005, 1006 ) ) ! tcx: (3,4,4)
   allocate ( g1, source = gen3(3,4,4) ( 'mno', 1007, 1008 ) ) ! tcx: (3,4,4)

   open ( 1, file = 'specific011lkk.1', form='unformatted', access='sequential' )

   write ( 1, iostat = stat, iomsg = msg ) b1, b2, c1, c2, g1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) ) error stop 1_4

   rewind 1

   deallocate ( b1, b2, c1, c2, g1 )
   allocate ( child(3,4) :: b1, c1 ) ! tcx: (3,4)
   allocate ( gen3(3,4,4)  :: b2, c2, g1 ) ! tcx: (3,4,4)

   read ( 1, iostat = stat, iomsg = msg )  b1, b2, c1, c2, g1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) ) error stop 2_4

   select type ( b1 )
      type is ( child(*,4) ) ! tcx: (*,4)
         if ( ( b1%c /= 'abc' ) .or. ( b1%i /= 1001 ) .or. ( c1%c /= 'ghi' ) .or. ( c1%i /= 1004 ) ) error stop 3_4
   end select

   select type ( b2 )
      type is ( gen3(*,4,4) ) ! tcx: (*,4,4)
         select type ( c2 )
            type is ( gen3(*,4,4) ) ! tcx: (*,4,4)
               if ( ( b2%c /= 'def' ) .or. ( b2%i /= 1002 ) .or. ( b2%j /= 1003 ) .or. &
                    ( c2%c /= 'jkl' ) .or. ( c2%i /= 1005 ) .or. ( c2%j /= 1006 ) .or. &
                    ( g1%c /= 'mno' ) .or. ( g1%i /= 1007 ) .or. ( g1%j /= 1008 ) ) error stop 4_4
         end select
   end select

   close (1, status = 'delete' )

end program

subroutine write (dtv, unit, iostat, iomsg)
   use m, only: child, gen3
   class(child(*,4)), intent(in) :: dtv ! tcx: (*,4)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   select type ( dtv )
      type is ( child(*,4) ) ! tcx: (*,4)
         write (unit, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
      type is ( gen3(*,4,4) ) ! tcx: (*,4,4)
         write (unit, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i, dtv%j
   end select

   iomsg = 'dtiowritec'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 3 changes
! type: child - added parameters (kchild_1) to invoke with (3,4) / declare with (*,4) - 10 changes
! type: gen3 - added parameters (kgen3_1) to invoke with (3,4,4) / declare with (*,4,4) - 9 changes
