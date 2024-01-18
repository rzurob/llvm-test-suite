!*  ===================================================================
!*
!*  TEST CASE NAME             : specific005kl
!*
!*  DATE                       : 2007-08-09 (original: 04/26/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Section 4.5.4 Type Bound Procedures (generic-binding)
!*                                 - Specific Binding
!*                                    - specific binding referring to a external procedure
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
   end type

   interface
      subroutine writebase (dtv, unit, iotype, v_list, iostat, iomsg)
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
      subroutine readbase (dtv, unit, iotype, v_list, iostat, iomsg)
         import base
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

end module

program specific005kl
   use m

   integer(4) :: stat
   character(200) :: msg

   class(base(:)), allocatable :: b1 ! tcx: (:)
   class(child(:,4)), pointer    :: c1 ! tcx: (:,4)

   allocate ( b1, source = base(3)('abc') ) ! tcx: (3)
   allocate ( c1, source = child(3,4)('def', 1001) ) ! tcx: (3,4)

   open ( 1, file = 'specific005kl.1', form='formatted', access='sequential' )

   write (1, *, iostat=stat, iomsg = msg)  b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) )    error stop 1_4

   write (1, *, iostat=stat, iomsg = msg)  c1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) )    error stop 2_4

   rewind 1

   read (1, *, iostat=stat, iomsg = msg)  c1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )     error stop 3_4

   read (1, *, iostat=stat, iomsg = msg)  b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )     error stop 4_4

   if ( ( b1%c /= 'def' ) .or. ( c1%c /= 'abc' ) .or. ( c1%i /= 9999 ) ) error stop 5_4

   close ( 1, status ='delete')

end program

subroutine writebase (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, child
   class(base(*)), intent(in) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)  :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   select type ( dtv )
      type is ( base(*) ) ! tcx: (*)
         write (unit, "(A3,1X,I4)", iostat=iostat, iomsg=iomsg) dtv%c, 9999
      type is ( child(*,4) ) ! tcx: (*,4)
         write (unit, "(A3,1X,I4)", iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
   end select

   iomsg = 'dtiowrite'

end subroutine

subroutine readbase (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, child
   class(base(*)), intent(inout) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)  :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   select type ( dtv )
      type is ( base(*) ) ! tcx: (*)
         read (unit, "(A3,5X)" , iostat=iostat, iomsg=iomsg) dtv%c
      type is ( child(*,4) ) ! tcx: (*,4)
         read (unit, "(A3,1X,I4)", iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
   end select

   iomsg = 'dtioread'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 8 changes
! type: child - added parameters (kchild_1) to invoke with (3,4) / declare with (*,4) - 4 changes
