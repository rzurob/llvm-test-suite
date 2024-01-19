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
!*                                    - specific binding referring to a external procedure, for both parent and child types
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

   interface
      subroutine writechild (dtv, unit, iotype, v_list, iostat, iomsg)
         import child
         class(child(*,4)), intent(in) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

   interface
      subroutine readchild (dtv, unit, iotype, v_list, iostat, iomsg)
         import child
         class(child(*,4)), intent(inout) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

end module

program specific006kl
   use m

   integer(4) :: stat
   character(200) :: msg

   class(base(:)), allocatable :: b1, b2 ! tcx: (:)
   class(child(:,4)), pointer    :: c1 ! tcx: (:,4)

   allocate ( b1, source = base(3)('abc') ) ! tcx: (3)
   allocate ( b2, source = child(3,4)('def', 1001) ) ! tcx: (3,4)
   allocate ( c1, source = child(3,4)('ghi', 2001) ) ! tcx: (3,4)

   open ( 1, file = 'specific006kl.1', form='formatted', access='sequential' )

   write (1, *, iostat=stat, iomsg = msg)  b1, b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) )    error stop 1_4

   write (1, *, iostat=stat, iomsg = msg)  c1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) )    error stop 2_4

   rewind 1

   deallocate ( c1 )
   allocate (child(3,4):: c1 ) ! tcx: child(3,4)

   read (1, *, iostat=stat, iomsg = msg)   b2, b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) )     error stop 3_4

   read (1, *, iostat=stat, iomsg = msg)   c1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) )     error stop 4_4

   select type ( b2 )
      type is ( child(*,4) ) ! tcx: (*,4)
         if ( ( b1%c /= 'def' ) .or. ( b2%c /= 'abc' ) .or. ( b2%i /= 9999 ) .or. ( c1%c /= 'ghi' ) .or. ( c1%i /= 2001 ) ) error stop 5_4
   end select

   close ( 1, status ='delete')

end program

subroutine writebase (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base
   class(base(*)), intent(in) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)  :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write (unit, "(A3,1X,I4)", iostat=iostat, iomsg=iomsg) dtv%c, 9999
   iomsg = 'dtiowriteb'

end subroutine

subroutine readbase (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base
   class(base(*)), intent(inout) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)  :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   read (unit, "(A3,5X)" , iostat=iostat, iomsg=iomsg) dtv%c
   iomsg = 'dtioreadb'

end subroutine

subroutine writechild (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: child
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
   use m, only: child
   class(child(*,4)), intent(inout) :: dtv ! tcx: (*,4)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)  :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   read (unit, "(A3,1X,I4)", iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
   iomsg = 'dtioreadc'

end subroutine




! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 6 changes
! type: child - added parameters (kchild_1) to invoke with (3,4) / declare with (*,4) - 8 changes
