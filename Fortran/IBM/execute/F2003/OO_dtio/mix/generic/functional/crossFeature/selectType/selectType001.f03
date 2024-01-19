!*  ===================================================================
!*
!*  DATE                       : 04/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : GENERIC BINDING:
!*                                  Cross Feature: Select Type Construct
!*                                    -  selector is a polymorphic scalar entity with formatted i/o

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

program selectType001
   use m

   integer :: stat
   character(200) :: msg

   class(base), pointer     :: b1
   class(base), allocatable :: b2
   class(child), allocatable, target :: c1

   allocate ( b1, source = base ( 'abc' ) )
   allocate ( b2, source = base ( 'def' ) )
   allocate ( c1, source = child ( 'ghi', 10001 ) )

   open ( 1, file = 'selectType001.1', form='formatted', access='sequential' )

   select type ( g => b1 )
      class is ( base )
         write ( 1, "(DT(3))", iostat = stat, iomsg = msg ) g
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 1_4
   end select

   select type ( h => b2 )
      type is ( base )
         write ( 1, "(DT(3))", iostat = stat, iomsg = msg ) h
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 2_4
   end select

   select type ( i => c1 )
      class is ( child )
         write ( 1, "(DT(3,5))", iostat = stat, iomsg = msg ) i
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) ) error stop 3_4
   end select

   deallocate ( b1 )
   b1 => c1
   deallocate ( b2 )
   allocate ( b2, source = child ( 'jkl', 100002 ) )

   select type ( h => b1 )
      class default
         write ( 1, "(DT(3,5))", iostat = stat, iomsg = msg ) h
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) ) error stop 4_4
   end select

   select type ( i => b2 )
      class default
         write ( 1, "(DT(3,6))", iostat = stat, iomsg = msg ) i
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) ) error stop 5_4
   end select

   rewind 1

   deallocate ( b2, c1 )
   allocate ( b1, b2, c1)

   select type ( g => b1 )
      class is ( base )
         read ( 1, "(DT(3))", iostat = stat, iomsg = msg ) g
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) ) error stop 6_4
   end select

   if ( b1%c /= 'abc' ) error stop 7_4

   select type ( h => b2 )
      type is ( base )
         read ( 1, "(DT(3))", iostat = stat, iomsg = msg ) h
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) ) error stop 8_4
   end select

   if ( b2%c /= 'def' ) error stop 9_4

   select type ( i => c1 )
      class is ( child )
         read ( 1, "(DT(3,5))", iostat = stat, iomsg = msg ) i
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) ) error stop 10_4
   end select

   if ( ( c1%c /= 'ghi' ) .or. ( c1%i /= 10001 ) ) error stop 11_4

   c1%c = 'xxx'
   c1%i = -999

   deallocate ( b1 )
   b1 => c1
   deallocate ( b2 )
   allocate ( child :: b2 )

   select type ( h => b1 )
      class default
         read ( 1, "(DT(3,5))", iostat = stat, iomsg = msg ) h
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) ) error stop 12_4
   end select

   select type ( b1 )
      type is ( child )
         if ( ( b1%c /= 'ghi' ) .or. ( b1%i /= 10001 ) .or. ( c1%c /= 'ghi' ) .or. ( c1%i /= 10001 ) ) error stop 13_4
   end select

   select type ( i => b2 )
      class default
         read ( 1, "(DT(3,6))", iostat = stat, iomsg = msg ) i
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) ) error stop 14_4
   end select

   select type ( b2 )
      type is ( child )
         if ( ( b2%c /= 'jkl' ) .or. ( b2%i /= 100002 ) ) error stop 15_4
   end select

   close ( 1, status ='delete')

end program
