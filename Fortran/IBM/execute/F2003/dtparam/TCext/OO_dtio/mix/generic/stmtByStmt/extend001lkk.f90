!*  ===================================================================
!*
!*  TEST CASE NAME             : extend001lkk
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
!*                                             - Extension type using parent's dtio generic binding (write)
!*                               adaptation: exposed kinds, length
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
   end type

   type, extends(base) :: child (kchild_1) ! kchild_1=4
      integer, kind :: kchild_1
      integer(kchild_1) :: i
   end type

   type, extends(child) :: gen3 (kgen3_1) ! kgen3_1=4
      integer, kind :: kgen3_1
      integer(kgen3_1) :: j
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
            type is ( gen3(*,4,4) ) ! tcx: (*,4,4)
               write (unit, *, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i, dtv%j
               iomsg = 'dtiowritegen3'
         end select

      end subroutine

end module

program extend001lkk
   use m

   class(base(:)), allocatable :: b1 ! tcx: (:)
   class(child(:,4)), pointer    :: c1 ! tcx: (:,4)
   class(gen3(:,4,4)), allocatable :: g1 ! tcx: (:,4,4)

   type ( child(3,4) ) :: c2 = child(3,4) ( 'DEF', 202 ) ! tcx: (3,4) ! tcx: (3,4)
   type ( gen3(3,4,4) )  :: g2 = gen3(3,4,4)  ( 'dEf', 302, 312 ) ! tcx: (3,4,4) ! tcx: (3,4,4)

   namelist /b1nml/ b1

   integer :: stat
   character(200) :: msg

   open ( 101, file = 'extend001lkk.1', form='formatted', access='sequential' )

   allocate ( b1, source = base(3) ('abc') ) ! tcx: (3)
   allocate ( c1, source = child(3,4)('ABC', 201 ) ) ! tcx: (3,4)
   allocate ( g1, source = gen3(3,4,4) ('aBc', 301, 311  ) ) ! tcx: (3,4,4)

   write ( 101, *, iostat = stat, iomsg = msg )         b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritebase' ) ) error stop 101_4

   write ( 101, *, iostat = stat, iomsg = msg )         c1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritechild' ) )error stop 2_4

   write ( 101, *, iostat = stat, iomsg = msg )         c2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritechild' ) )error stop 3_4

   write ( 101, *, iostat = stat, iomsg = msg )         g1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritegen3' ) ) error stop 4_4

   write ( 101, *, iostat = stat, iomsg = msg )         g2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritegen3' ) ) error stop 5_4

   deallocate ( b1, c1 )

   allocate ( b1, source = child(3,4) ( 'GHI', 203 ) ) ! tcx: (3,4)
   allocate ( c1, source = gen3(3,4,4) ( 'gHi', 303, 313 ) ) ! tcx: (3,4,4)

   write ( 101, "(DT)", iostat = stat, iomsg = msg )    b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritechild' ) )error stop 6_4

   write ( 101, "(DT)", iostat = stat, iomsg = msg )    c1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritegen3' ) ) error stop 7_4

   deallocate ( b1 )

   allocate ( b1, source = gen3(3,4,4) ( 'GHI', 304, 314 ) ) ! tcx: (3,4,4)

   write ( 101, b1nml, iostat = stat, iomsg = msg )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritegen3' ) ) error stop 8_4

end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 4 changes
! type: child - added parameters () to invoke with (3,) / declare with (*,) - 6 changes
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 4 changes
! type: child - added parameters (kchild_1) to invoke with (3,4) / declare with (*,4) - 6 changes
! type: gen3 - added parameters (kgen3_1) to invoke with (3,4,4) / declare with (*,4,4) - 7 changes
