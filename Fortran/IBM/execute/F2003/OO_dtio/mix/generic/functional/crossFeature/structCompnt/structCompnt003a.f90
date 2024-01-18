!*  ===================================================================
!*
!*  DATE                       : 04/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : GENERIC BINDING:
!*                                  Cross Feature: Structure Component
!*                                    -  non-polymorphic structure component with unformatted i/o
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

   type container
      class(base), allocatable :: b1
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

program structCompnt003a
   use m

   integer :: stat
   character(200) :: msg

   type(container) :: cc1
   class(container), allocatable :: cc2

   open ( 1, file = 'structCompnt003a.1', form='unformatted', access='sequential' )

   cc1 = container(base('abc'))
   allocate ( cc2, source = container(base('def')) )

   write ( 1, iostat = stat, iomsg = msg )        cc1%b1, cc2%b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) )               error stop 1_4

   deallocate ( cc2 )
   cc1 = container ( child ('ABC', 1001) )
   allocate ( cc2, source = container ( child ('DEF', 10002) ) )

   write ( 1, iostat = stat, iomsg = msg )    cc1%b1, cc2%b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) )               error stop 2_4

   rewind 1

   deallocate ( cc2 )
   cc1 = container(base())
   allocate ( cc2, source = container ( base() ) )

   read ( 1, iostat = stat, iomsg = msg )        cc2%b1, cc1%b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) )               error stop 3_4

   if ( ( cc1%b1%c /= 'def' ) .or. ( cc2%b1%c /= 'abc' ) )        error stop 4_4

   deallocate ( cc2 )
   cc1 = container ( child () )
   allocate ( cc2, source = container ( child () ) )

   read ( 1, iostat = stat, iomsg = msg )   cc2%b1, cc1%b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) )              error stop 5_4

   select type ( g => cc1%b1 )
      type is ( child )
         if ( ( g%c /= 'DEF' ) .or. ( g%i /= 10002 ) )           error stop 6_4
   end select

   select type ( g => cc2%b1 )
      type is ( child )
         if ( ( g%c /= 'ABC' ) .or. ( g%i /= 1001 ) )            error stop 7_4
   end select

   close ( 1, status ='delete')

end program
