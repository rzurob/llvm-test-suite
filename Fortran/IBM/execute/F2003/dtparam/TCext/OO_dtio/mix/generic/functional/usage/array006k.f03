!*  ===================================================================
!*
!*  DATE                       : 2007-08-13 (original: 04/26/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Usage of GENERIC BINDING
!*                                  - array variables of zero-storage
!*                                    with formatted I/O
!*                               adaptation: superfluous kind
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

   type base (kbase_1) ! kbase_1=4
      integer, kind :: kbase_1
      contains
         procedure, pass :: write => writeb
         procedure, pass :: read => readb
         generic :: write(formatted) => write
         generic :: read(formatted)  => read
   end type

   type, extends(base) :: child
      contains
         procedure, pass :: write => writec
         procedure, pass :: read => readc
   end type

   integer :: idx

   contains

      subroutine writeb (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(4)), intent(in) :: dtv ! tcx: (4)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         iomsg = 'dtiowriteb'

         idx = idx + 1

      end subroutine

      subroutine readb (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(4)), intent(inout) :: dtv ! tcx: (4)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         iomsg = 'dtioreadb'

         idx = idx + 1

      end subroutine

      subroutine writec (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child(4)), intent(in) :: dtv ! tcx: (4)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         iomsg = 'dtiowritec'

         idx = idx + 1

      end subroutine

      subroutine readc (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child(4)), intent(inout) :: dtv ! tcx: (4)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         iomsg = 'dtioreadc'

         idx = idx + 1

      end subroutine

end module

program array006k
   use m

   integer :: stat
   character(200) :: msg

   class(base(4)), allocatable :: b1(:), b2(:,:) ! tcx: (4)

   open ( 1, file = 'array006k.1', form='formatted', access='sequential' )

   allocate ( b1(3) )
   allocate ( child(4) :: b2(2,2) ) ! tcx: (4)

   idx = 0

   write ( 1, *, iostat = stat, iomsg = msg )        b1, '|'
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) .or. ( idx /= 3 ) ) error stop 101_4

   idx = 0

   write ( 1, "(DT)", iostat = stat, iomsg = msg )   b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) .or. ( idx /= 4 ) ) error stop 2_4

   rewind 1

   idx = 0

   read ( 1, *, iostat = stat, iomsg = msg )         b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) .or. ( idx /= 3 ) )  error stop 3_4

   idx = 0

   read ( 1, "(DT)", iostat = stat, iomsg = msg )    b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) .or. ( idx /= 4 ) )  error stop 4_4

   close ( 1, status ='delete')

end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 3 changes
! type: child - added parameters () to invoke with (4) / declare with (4) - 3 changes
