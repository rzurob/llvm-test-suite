!*  ===================================================================
!*
!*  TEST CASE NAME             : extend101lk
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
!*                                             - Extension type using parent's dtio generic binding (read)
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
         generic :: write(formatted) => write
         procedure, pass :: read => readbase
         generic :: read(formatted) => read
   end type

   type, extends(base) :: child (kchild_1) ! kchild_1=4
      integer, kind :: kchild_1
      integer(kchild_1) :: i = -99
   end type

   type, extends(child) :: gen3
      integer(kchild_1) :: j = -99
   end type

   contains

      subroutine readbase (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         select type ( dtv )
            type is ( base(*) ) ! tcx: (*)
               read (unit, *, iostat=iostat, iomsg=iomsg) dtv%c
               iomsg = 'dtioreadbase'
            type is ( child(*,4) ) ! tcx: (*,4)
               read (unit, *, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
               iomsg = 'dtioreadchild'
            type is ( gen3(*,4) ) ! tcx: (*,4)
               read (unit, *, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i, dtv%j
               iomsg = 'dtioreadgen3'
         end select

      end subroutine

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
            type is ( child(*,4) ) ! tcx: (*,4)
               write (unit, *, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
            type is ( gen3(*,4) ) ! tcx: (*,4)
               write (unit, *, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i, dtv%j
         end select

      end subroutine
end module

program extend101lk
   use m

   class(base(:)), allocatable :: b1 ! tcx: (:)
   class(child(:,4)), pointer    :: c1 ! tcx: (:,4)
   class(gen3(:,4)), allocatable :: g1 ! tcx: (:,4)

   type ( child(3,4) ) :: c2 = child(3,4) () ! tcx: (3,4) ! tcx: (3,4)
   type ( gen3(3,4) )  :: g2 = gen3(3,4)  () ! tcx: (3,4) ! tcx: (3,4)

   namelist /b1nml/ b1

   integer :: stat
   character(200) :: msg

   open ( 101, file = 'extend101lk.1', form='formatted', access='sequential' )

   allocate ( b1, source = base(3) () ) ! tcx: (3)
   allocate ( c1, source = child(3,4)() ) ! tcx: (3,4)
   allocate ( g1, source = gen3(3,4) ( ) ) ! tcx: (3,4)

   read ( 101, *, iostat = stat, iomsg = msg )         b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadbase' ) ) error stop 101_4

   read ( 101, *, iostat = stat, iomsg = msg )         c1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadchild' ) )error stop 2_4

   read ( 101, *, iostat = stat, iomsg = msg )         c2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadchild' ) )error stop 3_4

   read ( 101, *, iostat = stat, iomsg = msg )         g1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadgen3' ) ) error stop 4_4

   read ( 101, *, iostat = stat, iomsg = msg )         g2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadgen3' ) ) error stop 5_4

   print *, b1
   print *, c1
   print *, c2
   print *, g1
   print *, g2

   deallocate ( b1, c1 )

   allocate ( b1, source = child(3,4) () ) ! tcx: (3,4)
   allocate ( c1, source = gen3(3,4) () ) ! tcx: (3,4)

   read ( 101, "(DT)", iostat = stat, iomsg = msg )    b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadchild' ) )error stop 6_4

   read ( 101, "(DT)", iostat = stat, iomsg = msg )    c1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadgen3' ) ) error stop 7_4

   print *, b1
   print *, c1

   deallocate ( b1 )

   allocate ( b1, source = gen3(3,4) () ) ! tcx: (3,4)

   read ( 101, b1nml, iostat = stat, iomsg = msg )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadgen3' ) ) error stop 8_4

   print *, b1

end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 6 changes
! type: child - added parameters (kchild_1) to invoke with (3,4) / declare with (*,4) - 7 changes
! type: gen3 - added parameters () to invoke with (3,4) / declare with (*,4) - 8 changes
