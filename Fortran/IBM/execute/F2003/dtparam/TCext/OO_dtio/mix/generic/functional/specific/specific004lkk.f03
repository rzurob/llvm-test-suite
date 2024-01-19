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
!*                                    - specific binding available both in parent and extended types
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

   type, extends(base) :: child (kchild_1) ! kchild_1=4
      integer, kind :: kchild_1
      integer(kchild_1) :: i = -999
      contains
         procedure, pass :: write => writechild
         procedure, pass :: read => readchild
   end type

   type, extends(child) :: gen3 (kgen3_1) ! kgen3_1=4
      integer, kind :: kgen3_1
      integer(kgen3_1) :: j = -999
   end type

   contains

      subroutine writebase (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write ( unit, *, iostat = iostat , iomsg = iomsg ) dtv%c
         iomsg = 'dtiobasewrite'
      end subroutine

      subroutine readbase (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read ( unit, "(1X,A3)", iostat = iostat , iomsg = iomsg ) dtv%c
         iomsg = 'dtiobaseread'
      end subroutine

      subroutine writechild (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child(*,4)), intent(in) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         select type ( dtv )
            type is ( child(*,4) ) ! tcx: (*,4)
               write (unit, *, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
               iomsg = 'dtiochildwrite'
            type is ( gen3(*,4,4) ) ! tcx: (*,4,4)
               write (unit, *, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i, dtv%j
               iomsg = 'dtiogen3write'
         end select

      end subroutine

      subroutine readchild (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child(*,4)), intent(inout) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         select type ( dtv )
            type is ( child(*,4) ) ! tcx: (*,4)
               read (unit, "(1X,A3,1X,I4)", iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
               iomsg = 'dtiochildread'
            type is ( gen3(*,4,4) ) ! tcx: (*,4,4)
               read (unit, "(1X,A3,1X,I4,1X,I4)", iostat=iostat, iomsg=iomsg) dtv%c, dtv%i, dtv%j
               iomsg = 'dtiogen3read'
         end select

      end subroutine

end module

program specific004lkk
   use m

   class(base(:)), allocatable :: b1 ! tcx: (:)
   class(child(:,4)), pointer    :: c1 ! tcx: (:,4)
   class(gen3(:,4,4)), pointer     :: g1 ! tcx: (:,4,4)

   integer :: stat
   character(200) :: msg

   allocate ( b1, source = base(3)  ( 'abc' ) ) ! tcx: (3)
   allocate ( c1, source = child(3,4) ( 'def', 2001 ) ) ! tcx: (3,4)
   allocate ( g1, source = gen3(3,4,4)  ( 'ghi', 3001, 3002 ) ) ! tcx: (3,4,4)

   open ( 1, file = 'specific004lkk.1', form='formatted', access='sequential' )

   write ( 1, "(DT)", iostat = stat, iomsg = msg )  b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiobasewrite' ) )  error stop 1_4
   write ( 1, "(DT)", iostat = stat, iomsg = msg )  c1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiochildwrite' ) ) error stop 2_4
   write ( 1, "(DT)", iostat = stat, iomsg = msg )  g1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiogen3write' ) )  error stop 3_4

   deallocate ( b1, c1 )

   allocate ( b1, source = child(3,4) ( 'jkl', 2002 ) ) ! tcx: (3,4)
   allocate ( c1, source = gen3(3,4,4)  ( 'mno', 3003, 3004 ) ) ! tcx: (3,4,4)

   write ( 1, "(DT)", iostat = stat, iomsg = msg )  b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiochildwrite' ) ) error stop 4_4
   write ( 1, "(DT)", iostat = stat, iomsg = msg )  c1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiogen3write' ) )  error stop 5_4

   deallocate ( b1 )

   allocate ( b1, source = gen3(3,4,4) ( 'pqr', 3005, 3006 ) ) ! tcx: (3,4,4)
   write ( 1, "(DT)", iostat = stat, iomsg = msg )  b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiogen3write' ) )  error stop 6_4

   deallocate ( b1, c1, g1 )
   allocate   (base(3):: b1 ) ! tcx: base(3)
   allocate   (child(3,4):: c1 ) ! tcx: child(3,4)
   allocate   (gen3(3,4,4):: g1 ) ! tcx: gen3(3,4,4)

   rewind 1

   read ( 1, "(DT)", iostat = stat, iomsg = msg )  b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiobaseread' ) )  error stop 7_4
   read ( 1, "(DT)", iostat = stat, iomsg = msg )  c1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiochildread' ) ) error stop 8_4
   read ( 1, "(DT)", iostat = stat, iomsg = msg )  g1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiogen3read' ) )  error stop 9_4

   if ( b1%c /= 'abc' ) error stop 10_4
   if ( ( c1%c /= 'def' ) .or. ( c1%i /= 2001 ) ) error stop 11_4
   if ( ( g1%c /= 'ghi' ) .or. ( g1%i /= 3001 ) .or. ( g1%j /= 3002 ) )  error stop 12_4

   deallocate ( b1, c1 )

   allocate ( b1, source = child(3,4) () ) ! tcx: (3,4)
   allocate ( c1, source = gen3(3,4,4)  () ) ! tcx: (3,4,4)

   read ( 1, "(DT)", iostat = stat, iomsg = msg )  b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiochildread' ) ) error stop 13_4
   read ( 1, "(DT)", iostat = stat, iomsg = msg )  c1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiogen3read' ) )  error stop 14_4

   select type ( b1 )
      type is ( child(*,4) ) ! tcx: (*,4)
         if ( ( b1%c /= 'jkl' ) .or. ( b1%i /= 2002 ) ) error stop 15_4
   end select

   select type ( c1 )
      type is ( gen3(*,4,4) ) ! tcx: (*,4,4)
         if ( ( c1%c /= 'mno' ) .or. ( c1%i /= 3003 ) .or. ( c1%j /= 3004 ) )  error stop 16_4
   end select

   deallocate ( b1 )

   allocate ( b1, source = gen3(3,4,4) () ) ! tcx: (3,4,4)
   read ( 1, "(DT)", iostat = stat, iomsg = msg )  b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiogen3read' ) )  error stop 17_4

   select type ( b1 )
      type is ( gen3(*,4,4) ) ! tcx: (*,4,4)
         if ( ( b1%c /= 'pqr' ) .or. ( b1%i /= 3005 ) .or. ( b1%j /= 3006 ) )  error stop 18_4
   end select

   close ( 1, status ='delete')

end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 4 changes
! type: child - added parameters (kchild_1) to invoke with (3,4) / declare with (*,4) - 9 changes
! type: gen3 - added parameters (kgen3_1) to invoke with (3,4,4) / declare with (*,4,4) - 10 changes
