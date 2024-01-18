!*  ===================================================================
!*
!*  DATE                       : 2007-08-08 (original: 04/26/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : GENERIC BINDING:
!*                                  Cross Feature: Structure Component
!*                                    -  Parent/Grandparent structure component with formatted i/o
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

   type base (lbase1) ! lbase1=3
      integer, len :: lbase1
      character(lbase1) :: c = 'xxx'
      contains
         procedure, pass :: write => writeb
         procedure, pass :: read => readb
         generic :: write(formatted) => write
         generic :: read(formatted) => read
   end type

   type, extends(base) :: child (kchild1) ! kchild1=4
      integer, kind :: kchild1
      integer(kchild1) :: i = -999
      contains
         procedure, pass :: write => writec
         procedure, pass :: read => readc
   end type

   type, extends(child) :: gen3
      character(lbase1) :: s = 'xxx'
   end type

   contains

      subroutine writeb (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         character(20) :: fmt

         write (fmt, *) "(A", v_list(1), ")"
         write (unit, fmt, iostat=iostat, iomsg=iomsg) dtv%c

         iomsg = 'dtiowriteb'

      end subroutine

      subroutine writec (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child(*,4)), intent(in) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         character(20) :: fmt

         write (fmt, *) "(A", v_list(1), ",1X, I",v_list(2),")"

         write (unit, fmt, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i

         iomsg = 'dtiowritec'

      end subroutine

      subroutine readb (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         character(20) :: fmt

         write (fmt, *) "(A", v_list(1), ")"
         read (unit, fmt, iostat=iostat, iomsg=iomsg) dtv%c

         iomsg = 'dtioreadb'

      end subroutine

      subroutine readc (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child(*,4)), intent(inout) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         character(20) :: fmt

         write (fmt, *) "(A", v_list(1), ",1X, I",v_list(2),")"

         read (unit, fmt, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i

         iomsg = 'dtioreadc'

      end subroutine

end module

program structCompnt001lk
   use m

   integer :: stat
   character(200) :: msg

   type(child(3,4)) :: c1 = child(3,4)( 'abc', 10001 ) ! tcx: (3,4) ! tcx: (3,4)
   class(gen3(:,4)), allocatable :: g1 ! tcx: (:,4)

   open ( 1, file = 'structCompnt001lk.1', form='formatted', access='sequential' )

   allocate ( g1, source = gen3(3,4)('def', 100002, 'ghi') ) ! tcx: (3,4)

   write ( 1, "(DT(3))", iostat = stat, iomsg = msg )       c1%base
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 1_4

   write ( 1, "(DT(3))", iostat = stat, iomsg = msg )       g1%base
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 2_4

   write ( 1, "(DT(3))", iostat = stat, iomsg = msg )       g1%child%base
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 3_4

   write ( 1, "(DT(3,6))", iostat = stat, iomsg = msg )     g1%child
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) ) error stop 4_4

   rewind 1

   c1 = child(3,4)() ! tcx: (3,4)
   deallocate ( g1 )
   allocate (gen3(3,4):: g1 ) ! tcx: gen3(3,4)

   read ( 1, "(DT(3))", iostat = stat, iomsg = msg )       c1%base
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) )  error stop 5_4
   if ( c1%c /= 'abc' ) error stop 6_4

   read ( 1, "(DT(3))", iostat = stat, iomsg = msg )       g1%base
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) )  error stop 7_4
   if ( g1%c /= 'def' ) error stop 8_4
   g1%c = 'xxx'

   read ( 1, "(DT(3))", iostat = stat, iomsg = msg )       g1%child%base
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) )  error stop 9_4
   if ( g1%c /= 'def' ) error stop 10_4
   g1%c = 'xxx'

   read ( 1, "(DT(3,6))", iostat = stat, iomsg = msg )     g1%child
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) )  error stop 11_4
   if ( ( g1%c /= 'def' ) .or. ( g1%i /= 100002 ) )  error stop 12_4

   close ( 1, status ='delete')
end program


! Extensions to introduce derived type parameters:
! type: base - added parameters () to invoke with () / declare with () - 2 changes
! type: base - added parameters (lbase1) to invoke with (3) / declare with (*) - 2 changes
! type: child - added parameters (kchild1) to invoke with (3,4) / declare with (*,4) - 5 changes
! type: gen3 - added parameters () to invoke with (3,4) / declare with (*,4) - 2 changes
