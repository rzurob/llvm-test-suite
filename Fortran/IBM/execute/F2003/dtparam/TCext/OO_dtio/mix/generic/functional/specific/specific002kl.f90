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
!*                                    - deferred specific type bound procedure
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
         procedure(winf), deferred, pass :: write
         procedure(rinf), deferred, pass :: read
         generic :: write(formatted) => write
         generic :: read(formatted)  => read
   end type

   type, extends(base) :: child (kchild_1) ! kchild_1=4
      integer, kind :: kchild_1
      integer(kchild_1) :: i = -999
      contains
         procedure, pass :: write => writechild
         procedure, pass :: read  => readchild
   end type

   interface
      subroutine winf (dtv, unit, iotype, v_list, iostat, iomsg)
         import base
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

   interface
      subroutine rinf (dtv, unit, iotype, v_list, iostat, iomsg)
         import base
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

   contains

      subroutine writechild (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child(*,4)), intent(in) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, "(A3,1X,I4)", iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
         iomsg = 'dtiowrite'

      end subroutine

      subroutine readchild (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child(*,4)), intent(inout) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, "(A3,1X,I4)", iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
         iomsg = 'dtioread'

      end subroutine

end module

program specific002kl
   use m

   integer(4) :: stat
   character(200) :: msg


   class(base(:)), allocatable :: b1 ! tcx: (:)
   class(base(:)), pointer     :: b2 ! tcx: (:)

   type(child(3,4)), target      :: c1 ! tcx: (3,4)
   class(child(:,4)), pointer    :: c2 ! tcx: (:,4)

   namelist /n1/ b1, c1
   namelist /n2/ b2, c2

   allocate ( b1, source = child(3,4) ( 'abc', 1001 ) ) ! tcx: (3,4)
   allocate ( b2, source = child(3,4) ( 'def', 1002 ) ) ! tcx: (3,4)
   c1 = child(3,4) ( 'ghi', 1003 ) ! tcx: (3,4)
   allocate ( c2, source = child(3,4) ( 'jkl', 1004 ) ) ! tcx: (3,4)

   open ( 1, file = 'specific002kl.1', form='formatted', access='sequential' )

   write ( 1, n1, iostat = stat, iomsg = msg )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   write ( 1, n2, iostat = stat, iomsg = msg )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

   rewind 1

   deallocate ( b1, b2, c2 )
   allocate ( child(3,4) :: b1, b2, c2 ) ! tcx: (3,4)
   c1 = child(3,4)() ! tcx: (3,4)

   read ( 1, n1, iostat = stat, iomsg = msg )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 3_4

   read ( 1, n2, iostat = stat, iomsg = msg )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 4_4

   select type ( b1 )
      type is ( child(*,4) ) ! tcx: (*,4)
         if ( ( b1%c /= 'abc' ) .or. (b1%i /= 1001 ) ) error stop 5_4
   end select

   select type ( b2 )
      type is ( child(*,4) ) ! tcx: (*,4)
         if ( ( b2%c /= 'def' ) .or. (b2%i /= 1002 ) ) error stop 6_4
   end select

   if ( ( c1%c /= 'ghi' ) .or. ( c1%i /= 1003 ) ) error stop 7_4
   if ( ( c2%c /= 'jkl' ) .or. ( c2%i /= 1004 ) ) error stop 8_4

   deallocate ( b2, c2 )
   allocate ( child(3,4):: c2 ) ! tcx: child(3,4)
   b2 => c2

   rewind 1

   read ( 1, n2, iostat = stat, iomsg = msg )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )     error stop 9_4

   select type ( b2 )
      type is ( child(*,4) ) ! tcx: (*,4)
         if ( ( b2%c /= 'jkl' ) .or. (b2%i /= 1004 ) ) error stop 10_4
   end select

   if ( ( c2%c /= 'jkl' ) .or. ( c2%i /= 1004 ) )      error stop 11_4

end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 4 changes
! type: child - added parameters (kchild_1) to invoke with (3,4) / declare with (*,4) - 13 changes
