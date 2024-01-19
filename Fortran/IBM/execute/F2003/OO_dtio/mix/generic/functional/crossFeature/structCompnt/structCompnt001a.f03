!*  ===================================================================
!*
!*  DATE                       : 04/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : GENERIC BINDING:
!*                                  Cross Feature: Structure Component
!*                                    -  Parent/Grandparent structure component with unformatted i/o
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
         generic :: read(unformatted) => read
   end type

   type, extends(base) :: child
      integer(4) :: i = -999
      contains
         procedure, pass :: write => writec
         procedure, pass :: read => readc
   end type

   type, extends(child) :: gen3
      character(3) :: s = 'xxx'
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

      subroutine writec (dtv, unit, iostat, iomsg)
         class(child), intent(in) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i

         iomsg = 'dtiowritec'

      end subroutine

      subroutine readb (dtv, unit, iostat, iomsg)
         class(base), intent(inout) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, iostat=iostat, iomsg=iomsg) dtv%c

         iomsg = 'dtioreadb'

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

program structCompnt001a
   use m

   integer :: stat
   character(200) :: msg

   type(child) :: c1 = child( 'abc', 10001 )
   class(gen3), allocatable :: g1

   open ( 1, file = 'structCompnt001a.1', form='unformatted', access='sequential' )

   allocate ( g1, source = gen3('def', 100002, 'ghi') )

   write ( 1, iostat = stat, iomsg = msg )       c1%base
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 1_4

   write ( 1, iostat = stat, iomsg = msg )       g1%base
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 2_4

   write ( 1, iostat = stat, iomsg = msg )       g1%child%base
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 3_4

   write ( 1, iostat = stat, iomsg = msg )     g1%child
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) ) error stop 4_4

   rewind 1

   c1 = child()
   deallocate ( g1 )
   allocate ( g1 )

   read ( 1, iostat = stat, iomsg = msg )       c1%base
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) )  error stop 5_4
   if ( c1%c /= 'abc' ) error stop 6_4

   read ( 1, iostat = stat, iomsg = msg )       g1%base
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) )  error stop 7_4
   if ( g1%c /= 'def' ) error stop 8_4
   g1%c = 'xxx'

   read ( 1, iostat = stat, iomsg = msg )       g1%child%base
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) )  error stop 9_4
   if ( g1%c /= 'def' ) error stop 10_4
   g1%c = 'xxx'

   read ( 1, iostat = stat, iomsg = msg )     g1%child
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) )  error stop 11_4
   if ( ( g1%c /= 'def' ) .or. ( g1%i /= 100002 ) )  error stop 12_4

   close ( 1, status ='delete')

end program
