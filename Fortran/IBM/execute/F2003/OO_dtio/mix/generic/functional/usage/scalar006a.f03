!*  ===================================================================
!*
!*  DATE                       : 04/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Usage of GENERIC BINDING
!*                                  - scalar variables of zero-storage
!*                                    with unformatted I/O
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

   type base
      contains
         procedure, pass :: write => writeb
         procedure, pass :: read => readb
         generic :: write(unformatted) => write
         generic :: read(unformatted)  => read
   end type

   type, extends(base) :: child
      character(0) :: c = ''
      contains
         procedure, pass :: write => writec
         procedure, pass :: read => readc
   end type

   contains

      subroutine writeb (dtv, unit, iostat, iomsg)
         class(base), intent(in) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         iomsg = 'dtiowriteb'

      end subroutine

      subroutine readb (dtv, unit, iostat, iomsg)
         class(base), intent(inout) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         iomsg = 'dtioreadb'

      end subroutine

      subroutine writec (dtv, unit, iostat, iomsg)
         class(child), intent(in) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         iomsg = 'dtiowritec'

      end subroutine

      subroutine readc (dtv, unit, iostat, iomsg)
         class(child), intent(inout) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         iomsg = 'dtioreadc'

      end subroutine

end module

program scalar006a
   use m

   integer :: stat
   character(200) :: msg

   class(base), allocatable :: b1, b2

   open ( 1, file = 'scalar006a.1', form='unformatted', access='sequential' )

   allocate ( b1, source = base () )
   allocate ( b2, source = child() )

   write ( 1, iostat = stat, iomsg = msg )           b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 1_4

   write ( 1, iostat = stat, iomsg = msg )           b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) ) error stop 2_4

   rewind 1

   read ( 1, iostat = stat, iomsg = msg )            b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) )  error stop 3_4

   read ( 1, iostat = stat, iomsg = msg )            b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) )  error stop 4_4

   close ( 1, status ='delete')

end program
