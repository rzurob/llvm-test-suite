!*  ===================================================================
!*
!*  DATE                       : 04/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Usage of GENERIC BINDING
!*                                  - scalar unlimited polymorphic variable
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

   type base
      character(3) :: c = 'xxx'
      contains
         procedure, pass :: write => writeb
         procedure, pass :: read => readb
         generic :: write(formatted) => write
         generic :: read(formatted)  => read
   end type

   type, extends(base) :: child
      integer(4) :: i = -999
      contains
         procedure, pass :: write => writec
         procedure, pass :: read => readc
   end type

   contains

      subroutine writeb (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base), intent(in) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, "(A3)", iostat=iostat, iomsg=iomsg) dtv%c

         iomsg = 'dtiowriteb'

      end subroutine

      subroutine readb (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base), intent(inout) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, "(A3)" , iostat=iostat, iomsg=iomsg) dtv%c
         iomsg = 'dtioreadb'

      end subroutine

      subroutine writec (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child), intent(in) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, "(A3,1X,I4)", iostat=iostat, iomsg=iomsg) dtv%c, dtv%i

         iomsg = 'dtiowritec'

      end subroutine

      subroutine readc (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child), intent(inout) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, "(A3,1X,I4)" , iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
         iomsg = 'dtioreadc'

      end subroutine

end module

program scalar004
   use m

   class(*), allocatable :: u1
   class(*), pointer     :: u2

   integer :: i1, i2

   integer :: stat
   character(200) :: msg

   allocate ( u1, source = base ('abc') )
   allocate ( u2, source = child ('def',1001 ) )

   open ( 1, file = 'scalar004.1', form='formatted', access='sequential' )

   select type ( u1 )
      class is ( base )
         select type ( g => u2 )
            type is ( child )
               write ( 1, "(I4,1X,DT,I4,1X,DT)", iostat = stat, iomsg = msg )   1000, u1, 1002, g
               if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) )                error stop 1_4
         end select
   end select

   select type ( u1 )
      class is ( base )
         select type ( u2 )
            class is ( base )
               write ( 1, *, iostat = stat, iomsg = msg )                u1, u2
               if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) )         error stop 2_4
         end select
   end select

   rewind 1

   deallocate ( u1, u2 )
   allocate ( base  :: u1 )
   allocate ( child :: u2 )

   select type ( u1 )
      type is ( base )
         select type ( g => u2 )
            type is ( child )
               read ( 1, "(I4,1X,DT,I4,1X,DT)", iostat = stat, iomsg = msg )    i1, u1, i2, g
               if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) )                 error stop 3_4
               if ( ( i1 /= 1000 ) .or. ( u1%c /= 'abc' ) .or. ( i2 /= 1002 ) .or. ( g%c /= 'def' ) .or. ( g%i /= 1001 ) ) error stop 4_4
         end select
   end select

      select type ( u1 )
      type is ( base )
         select type ( g => u2 )
            type is ( child )
               read ( 1, "(2(1X,DT))", iostat = stat, iomsg = msg )                u1, g
               if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) )                    error stop 5_4
               if ( ( u1%c /= 'abc' ) .or. ( g%c /= 'def' ) .or. ( g%i /= 1001 ) ) error stop 6_4
         end select
   end select

   close ( 1, status ='delete')

end program