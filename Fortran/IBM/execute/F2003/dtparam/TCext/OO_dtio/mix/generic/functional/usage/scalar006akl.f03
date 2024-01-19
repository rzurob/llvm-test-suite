!*  ===================================================================
!*
!*  DATE                       : 2007-08-13 (original: 04/26/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Usage of GENERIC BINDING
!*                                  - scalar variables of zero-storage
!*                                    with unformatted I/O
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
         generic :: write(unformatted) => write
         generic :: read(unformatted)  => read
   end type

   type, extends(base) :: child (lchild_1) ! lchild_1=0
      integer, len :: lchild_1
      character(lchild_1) :: c = ''
      contains
         procedure, pass :: write => writec
         procedure, pass :: read => readc
   end type

   contains

      subroutine writeb (dtv, unit, iostat, iomsg)
         class(base(4)), intent(in) :: dtv ! tcx: (4)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         iomsg = 'dtiowriteb'

      end subroutine

      subroutine readb (dtv, unit, iostat, iomsg)
         class(base(4)), intent(inout) :: dtv ! tcx: (4)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         iomsg = 'dtioreadb'

      end subroutine

      subroutine writec (dtv, unit, iostat, iomsg)
         class(child(4,*)), intent(in) :: dtv ! tcx: (4,*)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         iomsg = 'dtiowritec'

      end subroutine

      subroutine readc (dtv, unit, iostat, iomsg)
         class(child(4,*)), intent(inout) :: dtv ! tcx: (4,*)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         iomsg = 'dtioreadc'

      end subroutine

end module

program scalar006akl
   use m

   integer :: stat
   character(200) :: msg

   class(base(4)), allocatable :: b1, b2 ! tcx: (4)

   open ( 1, file = 'scalar006akl.1', form='unformatted', access='sequential' )

   allocate ( b1, source = base(4) () ) ! tcx: (4)
   allocate ( b2, source = child(4,0)() ) ! tcx: (4,0)

   write ( 1, iostat = stat, iomsg = msg )           b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 101_4

   write ( 1, iostat = stat, iomsg = msg )           b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) ) error stop 2_4

   rewind 1

   read ( 1, iostat = stat, iomsg = msg )            b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) )  error stop 3_4

   read ( 1, iostat = stat, iomsg = msg )            b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) )  error stop 4_4

   close ( 1, status ='delete')

end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 4 changes
! type: child - added parameters (lchild_1) to invoke with (4,0) / declare with (4,*) - 3 changes
