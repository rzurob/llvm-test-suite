!*  ===================================================================
!*
!*  DATE                       : 04/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : GENERIC BINDING:
!*                                  Cross Feature: Select Type Construct
!*                                    -  selector is a unlimited polymorphic scalar/array entity with formatted i/o

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

         character(20) :: fmt

         write (fmt, *) "(A", v_list(1), ")"
         write (unit, fmt, iostat=iostat, iomsg=iomsg) dtv%c

         iomsg = 'dtiowriteb'

      end subroutine

      subroutine readb (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base), intent(inout) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         character(20) :: fmt

         write (fmt, *) "(A", v_list(1), ")"

         read (unit, fmt , iostat=iostat, iomsg=iomsg) dtv%c
         iomsg = 'dtioreadb'

      end subroutine

      subroutine writec (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child), intent(in) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         character(20) :: fmt

         write (fmt, *) "(A", v_list(1), ",1X, I",v_list(2),")"

         write (unit, fmt, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i

         iomsg = 'dtiowritec'

      end subroutine

      subroutine readc (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child), intent(inout) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         character(20) :: fmt
         write (fmt, *) "(A", v_list(1), ",1X, I",v_list(2),")"

         read (unit, fmt , iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
         iomsg = 'dtioreadc'

      end subroutine

end module

program selectType003
   use m

   integer :: stat
   character(200) :: msg

   class(*), pointer     :: b1
   class(*), allocatable :: b2
   class(*), allocatable :: c1(:)

   allocate ( b1, source = base('abc') )
   allocate ( b2, source = base('ABC') )
   allocate ( c1(2), source = (/ child('abc',10001 ), child('def',100002 ) /) )

   open ( 1, file = 'selectType003.1', form='formatted', access='stream' )

   select type ( g => b1 )
      class is ( base )
         write ( 1, "(DT(3))", iostat = stat, iomsg = msg, pos = 1 )    g
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 1_4
   end select

   select type ( h => b2 )
      type is ( base )
         write ( 1, "(DT(3))", iostat = stat, iomsg = msg, pos = 4 )    h
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 2_4
   end select

   select type ( i => c1 )
      class is ( base )
         write ( 1, "(DT(3,5),DT(3,6))", iostat = stat, iomsg = msg, pos=7 )  i
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) ) error stop 3_4
   end select

   deallocate ( b1, b2, c1 )

   allocate ( base :: b1, b2)
   allocate ( child :: c1(2) )

   select type ( g => b1 )
      class is ( base )
         read ( 1, "(DT(3))", iostat = stat, iomsg = msg, pos = 1 )    g
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) ) error stop 4_4
         if ( g%c /= 'abc' ) error stop 5_4

   end select

   select type ( h => b2 )
      type is ( base )
         read ( 1, "(DT(3))", iostat = stat, iomsg = msg, pos = 4 )    h
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) ) error stop 6_4
         if ( h%c /= 'ABC' ) error stop 7_4
   end select

   select type ( i => c1 )
      class is ( base )
         read ( 1, "(DT(3,5),DT(3,6))", iostat = stat, iomsg = msg, pos=7 )  i
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) ) error stop 8_4
   end select

   select type ( j => c1 )
      type is ( child )
         if ( ( j(1)%c /= 'abc' ) .or.  ( j(2)%c /= 'def' ) .or. ( j(1)%i /= 10001 ) .or.  ( j(2)%i /= 100002 ) ) error stop 9_4

   end select

   close ( 1, status ='delete')

end program
