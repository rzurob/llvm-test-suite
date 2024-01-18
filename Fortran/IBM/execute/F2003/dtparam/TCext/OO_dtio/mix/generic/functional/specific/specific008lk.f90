!*  ===================================================================
!*
!*  DATE                       : 2007-08-09 (original: 04/26/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Section 4.5.4 Type Bound Procedures (generic-binding)
!*                                 - Specific Binding
!*                                    - multiple type hierarchy, and all defined in different modules
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

   type base (lbase_1) ! lbase_1=3
      integer, len :: lbase_1
      character(lbase_1) :: c = 'xxx'
      contains
         procedure, pass :: write => writebase
         procedure, pass :: read => readbase
         generic :: write(formatted) => write
         generic :: read(formatted)  => read
   end type

   contains

      subroutine writebase (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, "(A3)", iostat=iostat, iomsg=iomsg) dtv%c
         iomsg = 'dtiowriteb'

      end subroutine

      subroutine readbase (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, "(A3)" , iostat=iostat, iomsg=iomsg)    dtv%c
         iomsg = 'dtioreadb'

      end subroutine

end module

module m1
   use m, only: base

   type, extends(base) :: child (kchild_1) ! kchild_1=4
      integer, kind :: kchild_1
      integer(kchild_1) :: i = -999
      contains
         procedure, pass :: write => writechild
         procedure, pass :: read => readchild
   end type

   contains

      subroutine writechild (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child(*,4)), intent(in) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, "(A3,1X,I4)", iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
         iomsg = 'dtiowritec'

      end subroutine

      subroutine readchild (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child(*,4)), intent(inout) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, "(A3,1X,I4)", iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
         iomsg = 'dtioreadc'

      end subroutine

end module

module m2
   use m1

   type, extends(child) :: gen3
      integer(kchild_1) :: j = -999
      contains
         procedure, pass :: write => writegen3
         procedure, pass :: read => readgen3
   end type

   contains

      subroutine writegen3 (dtv, unit, iotype, v_list, iostat, iomsg)
         class(gen3(*,4)), intent(in) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, "(A3,1X,I4,1X,I4)", iostat=iostat, iomsg=iomsg) dtv%c, dtv%i, dtv%j
         iomsg = 'dtiowriteg'

      end subroutine

      subroutine readgen3 (dtv, unit, iotype, v_list, iostat, iomsg)
         class(gen3(*,4)), intent(inout) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, "(A3,1X,I4,1X,I4)", iostat=iostat, iomsg=iomsg) dtv%c, dtv%i, dtv%j
         iomsg = 'dtioreadg'

      end subroutine

end module

program specific008lk
   use m2

   integer(4) :: stat
   character(200) :: msg

   class(base(:)), allocatable :: b1, b2, b3 ! tcx: (:)
   class(child(:,4)), pointer    :: c1, c2 ! tcx: (:,4)
   class(gen3(:,4)), pointer     :: g1 ! tcx: (:,4)

   allocate ( b1, source = base(3) ('abc') ) ! tcx: (3)
   allocate ( b2, source = child(3,4)('def', 1001) ) ! tcx: (3,4)
   allocate ( b3, source = gen3(3,4) ('ghi', 1002, 1003) ) ! tcx: (3,4)
   allocate ( c1, source = child(3,4)('ABC', 2001) ) ! tcx: (3,4)
   allocate ( c2, source = gen3(3,4) ('DEF', 2002, 2003) ) ! tcx: (3,4)
   allocate ( g1, source = gen3(3,4) ('GHI', 3001, 3002) ) ! tcx: (3,4)

   open ( 1, file = 'specific008lk.1', form='formatted', access='sequential' )

   write (1, *, iostat=stat, iomsg = msg)  b1, b2, b3
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteg' ) )    error stop 1_4

   write (1, *, iostat=stat, iomsg = msg)  c1, c2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteg' ) )    error stop 2_4

   write (1, *, iostat=stat, iomsg = msg)  g1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteg' ) )    error stop 3_4

   rewind 1

   deallocate ( b1, b2, b3, c1, c2, g1 )

   allocate (base(3):: b1 ) ! tcx: base(3)
   allocate ( child(3,4) :: b2, c1 ) ! tcx: (3,4)
   allocate ( gen3(3,4) :: b3, c2, g1 ) ! tcx: (3,4)

   read (1, *, iostat=stat, iomsg = msg)   b1, b2, b3
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadg' ) )     error stop 4_4

   read (1, *, iostat=stat, iomsg = msg)   c1, c2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadg' ) )     error stop 5_4

   read (1, *, iostat=stat, iomsg = msg)   g1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadg' ) )     error stop 6_4

   select type ( b2 )
      type is ( child(*,4) ) ! tcx: (*,4)
         select type ( b3 )
            type is ( gen3(*,4) ) ! tcx: (*,4)
               if ( ( b1%c /= 'abc' ) .or. ( b2%c /= 'def' ) .or. ( b2%i /= 1001 ) .or. ( b3%c /= 'ghi' ) .or. ( b3%i /= 1002 ) .or. ( b3%j /= 1003 ) ) error stop 7_4
         end select
   end select

   select type ( c2 )
      type is ( gen3(*,4) ) ! tcx: (*,4)
         if ( ( c1%c /= 'ABC' ) .or. ( c1%i /= 2001 ) .or. ( c2%c /= 'DEF' ) .or. ( c2%i /= 2002 ) .or. ( c2%j /= 2003 ) ) error stop 8_4
   end select

   if ( ( g1%c /= 'GHI' ) .or. ( g1%i /= 3001 ) .or. ( g1%j /= 3002 ) ) error stop 9_4
   close ( 1, status ='delete')

end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 4 changes
! type: child - added parameters (kchild_1) to invoke with (3,4) / declare with (*,4) - 7 changes
! type: gen3 - added parameters () to invoke with (3,4) / declare with (*,4) - 9 changes
