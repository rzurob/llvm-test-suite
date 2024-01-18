!*  ===================================================================
!*
!*  TEST CASE NAME             : scalar006lk
!*
!*  DATE                       : 2007-08-13 (original: 04/26/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Usage of GENERIC BINDING
!*                                  - scalar variables of zero-storage
!*                                    with formatted I/O
!*                               adaptation: superfluous length, kind
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

   type base (lbase_1) ! lbase_1=0
      integer, len :: lbase_1
      contains
         procedure, pass :: write => writeb
         procedure, pass :: read => readb
         generic :: write(formatted) => write
         generic :: read(formatted)  => read
   end type

   type, extends(base) :: child (kchild_1) ! kchild_1=4
      integer, kind :: kchild_1
      contains
         procedure, pass :: write => writec
         procedure, pass :: read => readc
   end type

   contains

      subroutine writeb (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         iomsg = 'dtiowriteb'

      end subroutine

      subroutine readb (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         iomsg = 'dtioreadb'

      end subroutine

      subroutine writec (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child(*,4)), intent(in) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         iomsg = 'dtiowritec'

      end subroutine

      subroutine readc (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child(*,4)), intent(inout) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         iomsg = 'dtioreadc'

      end subroutine

end module

program scalar006lk
   use m

   integer :: stat
   character(200) :: msg

   class(base(:)), allocatable :: b1, b2 ! tcx: (:)

   open ( 1, file = 'scalar006lk.1', form='formatted', access='sequential' )

   allocate ( b1, source = base(0) () ) ! tcx: (0)
   allocate ( b2, source = child(0,4)() ) ! tcx: (0,4)

   write ( 1, *, iostat = stat, iomsg = msg )        b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 101_4

   write ( 1, "(DT)", iostat = stat, iomsg = msg )   b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) ) error stop 2_4

   rewind 1

   read ( 1, '(DT)', iostat = stat, iomsg = msg )         b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) )  error stop 3_4

   read ( 1, "(DT)", iostat = stat, iomsg = msg )    b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) )  error stop 4_4

   close ( 1, status ='delete')

end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (0) / declare with (*) - 4 changes
! type: child - added parameters (kchild_1) to invoke with (0,4) / declare with (*,4) - 3 changes
