!*  ===================================================================
!*
!*  DATE                       : 2007-08-12 (original: 04/26/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Usage of GENERIC BINDING
!*                                  - array variables of zero-storage
!*                                    with formatted I/O
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

   type, extends(base) :: child (lchild_1) ! lchild_1=0
      integer, len :: lchild_1
      contains
         procedure, pass :: write => writec
         procedure, pass :: read => readc
   end type

   integer :: idx

   contains

      subroutine writeb (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         iomsg = 'dtiowriteb'

         idx = idx + 1

      end subroutine

      subroutine readb (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         iomsg = 'dtioreadb'

         idx = idx + 1

      end subroutine

      subroutine writec (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child(*,*)), intent(in) :: dtv ! tcx: (*,*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         iomsg = 'dtiowritec'

         idx = idx + 1

      end subroutine

      subroutine readc (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child(*,*)), intent(inout) :: dtv ! tcx: (*,*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         iomsg = 'dtioreadc'

         idx = idx + 1

      end subroutine

end module

program array006ll
   use m

   integer :: stat
   character(200) :: msg

   class(base(:)), allocatable :: b1(:), b2(:,:) ! tcx: (:)

   open ( 1, file = 'array006ll.1', form='formatted', access='sequential' )

   allocate ( base(0):: b1(3) ) ! tcx: base(0)
   allocate ( child(0,0) :: b2(2,2) ) ! tcx: (0,0)

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
! type: base - added parameters (lbase_1) to invoke with (0) / declare with (*) - 3 changes
! type: child - added parameters (lchild_1) to invoke with (0,0) / declare with (*,*) - 3 changes
