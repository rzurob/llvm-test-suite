!*  ===================================================================
!*
!*  DATE                       : 04/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Usage of GENERIC BINDING
!*                                  - scalar derived type parameter
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
      character(3) :: c = 'xxx'
      contains
         procedure, pass :: write => writeb
         procedure, pass :: read => readb
         generic :: write(unformatted) => write
         generic :: read(unformatted)  => read
   end type

   type, extends(base) :: child
      integer(4) :: i = -999
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

         write (unit, iostat=iostat, iomsg=iomsg) dtv%c

         iomsg = 'dtiowriteb'

      end subroutine

      subroutine readb (dtv, unit, iostat, iomsg)
         class(base), intent(inout) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, iostat=iostat, iomsg=iomsg) dtv%c
         iomsg = 'dtioreadb'

      end subroutine

      subroutine writec (dtv, unit, iostat, iomsg)
         class(child), intent(in) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i

         iomsg = 'dtiowritec'

      end subroutine

      subroutine readc (dtv, unit, iostat, iomsg)
         class(child), intent(inout) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
         iomsg = 'dtioreadc'

      end subroutine

end module

program scalar008a
   use m

   type(base), parameter  :: b1 = base('ibm')
   type(base), parameter  :: b2 = base('IBM')
   type(child), parameter :: c1 = child('ftn',2003)
   type(child), parameter :: c2 = child('FTN',2004)

   type(base)  :: b11, b12
   type(child) :: c11, c12

   integer :: stat
   character(200) :: msg

   open ( 1, file = 'scalar008a.1', form='unformatted', access='sequential' )

   write ( 1, iostat = stat, iomsg = msg )     b1, c1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) )   error stop 1_4

   write ( 1, iostat = stat, iomsg = msg )          c2, b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) )   error stop 2_4

   rewind 1

   read ( 1, iostat = stat, iomsg = msg )            b11, c11
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) )          error stop 3_4
   read ( 1, iostat = stat, iomsg = msg )   c12, b12
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) )          error stop 4_4

   if ( ( b11%c /= 'ibm' ) .or. ( c11%c /= 'ftn' ) .or. ( c11%i /= 2003 ) .or. &
        ( b12%c /= 'IBM' ) .or. ( c12%c /= 'FTN' ) .or. ( c12%i /= 2004 ) ) error stop 5_4

   close ( 1, status ='delete')

end program