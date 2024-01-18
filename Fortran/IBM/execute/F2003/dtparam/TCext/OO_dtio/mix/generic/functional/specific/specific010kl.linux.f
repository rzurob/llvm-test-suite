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
!*                                         - deferred binding in parent type, and child
!*                                           type has specific type bound in external procedure
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
         procedure, pass :: write => mywrite
         procedure, pass :: read => myread
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
      subroutine mywrite (dtv, unit, iostat, iomsg)
         import child
         class(child(*,4)), intent(in) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

   interface
      subroutine myread (dtv, unit, iostat, iomsg)
         import child
         class(child(*,4)), intent(inout) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

end module

program specific010kl
   use m

   integer(4) :: stat
   character(200) :: msg

   class(base(:)), allocatable  :: b1 ! tcx: (:)
   class(child(:,4)), pointer     :: c1 ! tcx: (:,4)
   type(child(3,4))               :: c2 = child(3,4) ( 'ghi', 1003 ) ! tcx: (3,4) ! tcx: (3,4)

   allocate ( b1, source = child(3,4) ( 'abc', 1001 ) ) ! tcx: (3,4)
   allocate ( c1, source = child(3,4) ( 'def', 1002 ) ) ! tcx: (3,4)

   open ( 1, file = 'specific010kl.1', form='unformatted', access='sequential' )

   write ( 1, iostat = stat, iomsg = msg ) b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) ) error stop 1_4

   write ( 1, iostat = stat, iomsg = msg ) c1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) ) error stop 2_4

   write ( 1, iostat = stat, iomsg = msg ) c2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) ) error stop 3_4

   rewind 1

   read ( 1, iostat = stat, iomsg = msg ) c2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) ) error stop 4_4

   read ( 1, iostat = stat, iomsg = msg ) b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) ) error stop 5_4

   read ( 1, iostat = stat, iomsg = msg ) c1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) ) error stop 6_4

   select type ( b1 )
      type is ( child(*,4) ) ! tcx: (*,4)
         if ( ( c2%c /= 'abc' ) .or. ( c2%i /= 1001 ) .or. ( b1%c /= 'def' ) .or. ( b1%i /= 1002 ) .or. &
              ( c1%c /= 'ghi' ) .or. ( c1%i /= 1003 )  ) error stop 7_4
   end select

   close (1, status = 'delete' )

end program

subroutine mywrite (dtv, unit, iostat, iomsg)
   use m, only: child
   class(child(*,4)), intent(in) :: dtv ! tcx: (*,4)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write (unit, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
   iomsg = 'dtiowritec'

end subroutine

subroutine myread (dtv, unit, iostat, iomsg)
   use m, only: child
   class(child(*,4)), intent(inout) :: dtv ! tcx: (*,4)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   read (unit, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
   iomsg = 'dtioreadc'

end subroutine



! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 3 changes
! type: child - added parameters (kchild_1) to invoke with (3,4) / declare with (*,4) - 10 changes
