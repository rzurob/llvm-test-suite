!*  ===================================================================
!*
!*  DATE                       : 2007-08-13 (original: 04/26/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Section 4.5.4 Type Bound Procedures (generic-binding)
!*                               Stmt by Stmt: (Pg.60 14-15) An extended type includes all of the type
!*                                                           parameters, all of the components, and the
!*                                                           nonoverridden (4.5.6.2) nonfinal procedure
!*                                                           bindings of its parent type.
!*
!*                                             - Extension type define a specific binding overridding parent's specific binding
!*                               adaptation: exposed kind, length
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

   type :: base (lbase_1) ! lbase_1=3
      integer, len :: lbase_1
      character(lbase_1) :: c = 'xxx'
      contains
         procedure, pass :: write => writebase
         procedure, pass :: read => readbase
         generic :: write(formatted) => write
         generic :: read(formatted) => read
   end type

   type, extends(base) :: child (kchild_1) ! kchild_1=4
      integer, kind :: kchild_1
      integer(kchild_1) :: i = -999
      contains
         procedure, pass :: read => readchild    !<- override parent's read binding
   end type

   contains

      subroutine writebase (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         select type ( dtv )
            type is ( base(*) ) ! tcx: (*)
               write (unit, *, iostat=iostat, iomsg=iomsg) dtv%c
               iomsg = 'dtiowritebase'
            type is ( child(*,4) ) ! tcx: (*,4)
               write (unit, *, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
               iomsg = 'dtiowritechild'
         end select

      end subroutine


      subroutine readbase (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, *, iostat=iostat, iomsg=iomsg) dtv%c
         iomsg = 'dtioreadbase'

      end subroutine

      subroutine readchild (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child(*,4)), intent(inout) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, *, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
         iomsg = 'dtioreadchild'

      end subroutine

end module

program extend002kl
   use m

   class(base(:)), allocatable :: b1 ! tcx: (:)
   class(child(:,4)), pointer    :: c1 ! tcx: (:,4)

   integer :: stat
   character(200) :: msg

   open ( 101, file = 'extend002kl.1', form='formatted', access='sequential' )

   allocate ( b1, source = base(3) ('abc') ) ! tcx: (3)
   allocate ( c1, source = child(3,4)('ABC', 201 ) ) ! tcx: (3,4)

   write ( 101, *, iostat = stat, iomsg = msg )         b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritebase' ) ) error stop 101_4

   write ( 101, *, iostat = stat, iomsg = msg )         c1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritechild' ) )error stop 2_4

   deallocate ( b1 )
   allocate ( b1, source = child(3,4) ( 'DEF', 202 ) ) ! tcx: (3,4)

   write ( 101, *, iostat = stat, iomsg = msg )         b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritechild' ) )error stop 3_4

   rewind 101

   deallocate ( b1, c1 )
   allocate (base(3):: b1 ) ! tcx: base(3)
   allocate (child(3,4):: c1 ) ! tcx: child(3,4)

   read ( 101, *, iostat = stat, iomsg = msg )         b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadbase' ) ) error stop 4_4

   if ( b1%c /= 'abc' ) error stop 5_4

   read ( 101, *, iostat = stat, iomsg = msg )         c1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadchild' ) )error stop 6_4

   if ( ( c1%c /= 'ABC' ) .or. ( c1%i /= 201 ) )       error stop 7_4

   deallocate ( b1 )
   allocate ( b1, source = child(3,4) () ) ! tcx: (3,4)

   read ( 101, *, iostat = stat, iomsg = msg )         b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadchild' ) )error stop 8_4

   select type ( b1 )
      type is ( child(*,4) ) ! tcx: (*,4)
         if ( ( b1%c /= 'DEF' ) .or. ( b1%i /= 202 ) )       error stop 9_4
   end select

   close ( 101, status = 'delete' )

end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 5 changes
! type: child - added parameters (kchild_1) to invoke with (3,4) / declare with (*,4) - 7 changes
