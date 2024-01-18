!*  ===================================================================
!*
!*  DATE                       : 04/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Usage of GENERIC BINDING
!*                                  - array variables of zero-storage
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

   integer :: idx

   contains

      subroutine writeb (dtv, unit, iostat, iomsg)
         class(base), intent(in) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         iomsg = 'dtiowriteb'

         idx = idx + 1

      end subroutine

      subroutine readb (dtv, unit, iostat, iomsg)
         class(base), intent(inout) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         iomsg = 'dtioreadb'

         idx = idx + 1

      end subroutine

      subroutine writec (dtv, unit, iostat, iomsg)
         class(child), intent(in) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         iomsg = 'dtiowritec'

         idx = idx + 1

      end subroutine

      subroutine readc (dtv, unit, iostat, iomsg)
         class(child), intent(inout) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         iomsg = 'dtioreadc'

         idx = idx + 1

      end subroutine

end module

program array006a
   use m

   integer :: stat
   character(200) :: msg

   class(base), allocatable :: b1(:), b2(:,:)

   open ( 1, file = 'array006a.1', form='unformatted', access='sequential' )

   allocate ( b1(3) )
   allocate ( child :: b2(2,2) )

   idx = 0

   write ( 1, iostat = stat, iomsg = msg )        b1((/1,2,2,3/))
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) .or. ( idx /= 4 ) ) error stop 1_4

   idx = 0

   write ( 1, iostat = stat, iomsg = msg )        b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) .or. ( idx /= 4 ) ) error stop 2_4

   rewind 1

   idx = 0

   read ( 1, iostat = stat, iomsg = msg )         b1(1:3)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) .or. ( idx /= 3 ) )  error stop 3_4

   idx = 0

   read ( 1, iostat = stat, iomsg = msg )         b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) .or. ( idx /= 4 ) )  error stop 4_4

   close ( 1, status ='delete')

end program
