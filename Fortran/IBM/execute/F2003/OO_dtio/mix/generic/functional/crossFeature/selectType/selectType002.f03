!*  ===================================================================
!*
!*  DATE                       : 04/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : GENERIC BINDING:
!*                                  Cross Feature: Select Type Construct
!*                                    -  selector is a polymorphic array entity with formatted i/o

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

program selectType002
   use m

   integer :: stat
   character(200) :: msg

   class(base), pointer     :: b1(:)
   class(base), allocatable :: b2(:)
   class(child), allocatable, target :: c1(:)

   allocate ( b1(3), source = (/ base('abc'), base('def'), base('ghi') /) )
   allocate ( b2(3), source = (/ base('ABC'), base('DEF'), base('GHI') /))
   allocate ( c1(2), source = (/ child('abc',10001 ), child('def',100002 ) /) )

   open ( 1, file = 'selectType002.1', form='formatted', access='direct', recl = 30 )

   select type ( g => b1 )
      class is ( base )
         write ( 1, "(DT(3))", iostat = stat, iomsg = msg , rec = 5 )    g
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 1_4
   end select

   select type ( h => b2 )
      type is ( base )
         write ( 1, "(3(DT(3)))", iostat = stat, iomsg = msg , rec = 4 ) h
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 2_4
   end select

   select type ( i => c1 )
      class is ( child )
         write ( 1, "(DT(3,5),DT(3,6))", iostat = stat, iomsg = msg , rec = 3 )  i
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) ) error stop 3_4
   end select

   deallocate ( b1 )
   b1 => c1
   deallocate ( b2 )
   allocate ( b2(3), source = (/ child ( 'jkl', 1002 ), child ( 'mno', 101 ), child ( 'pqr', 10 ) /) )

   select type ( h => b1 )
      class default
         write ( 1, "(DT(3,5),DT(3,6))", iostat = stat, iomsg = msg, rec = 2 ) h
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) ) error stop 4_4
   end select

   select type ( i => b2 )
      class default
         write ( 1, "(DT(3,4),DT(3,3),DT(3,2))", iostat = stat, iomsg = msg, rec = 1 ) i
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) ) error stop 5_4
   end select

   deallocate ( b2, c1 )
   allocate ( b1(3), b2(3), c1(2) )

   select type ( g => b1 )
      class is ( base )
         read ( 1, "(DT(3))", iostat = stat, iomsg = msg , rec = 5  ) g
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) ) error stop 6_4
   end select

   if ( ( b1(1)%c /= 'abc' ) .or.  ( b1(2)%c /= 'def' ) .or.  ( b1(3)%c /= 'ghi' ) ) error stop 7_4

   select type ( h => b2 )
      type is ( base )
         read ( 1, "(3(DT(3)))", iostat = stat, iomsg = msg , rec = 4 ) h
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) ) error stop 8_4
   end select

   if ( ( b2(1)%c /= 'ABC' ) .or.  ( b2(2)%c /= 'DEF' ) .or.  ( b2(3)%c /= 'GHI' ) ) error stop 9_4

   select type ( i => c1 )
      class is ( child )
         read ( 1, "(DT(3,5),DT(3,6))", iostat = stat, iomsg = msg , rec = 3 ) i
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) ) error stop 10_4
   end select

   if ( ( c1(1)%c /= 'abc' ) .or. ( c1(1)%i /= 10001 ) .or. &
        ( c1(2)%c /= 'def' ) .or. ( c1(2)%i /= 100002 ) ) error stop 11_4

   c1(1)%c = 'xxx'
   c1(1)%i = -999
   c1(2)%c = 'xxx'
   c1(2)%i = -999

   deallocate ( b1 )
   b1 => c1
   deallocate ( b2 )
   allocate ( child :: b2(3) )

   select type ( h => b1 )
      class default
         read ( 1, "(DT(3,5),DT(3,6))", iostat = stat, iomsg = msg , rec = 2  ) h
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) ) error stop 12_4
   end select

   select type ( b1 )
      type is ( child )
         if ( ( c1(1)%c /= 'abc' ) .or. ( c1(1)%i /= 10001 )  .or. &
              ( c1(2)%c /= 'def' ) .or. ( c1(2)%i /= 100002 ) .or. &
              ( b1(1)%c /= 'abc' ) .or. ( b1(1)%i /= 10001 )  .or. &
              ( b1(2)%c /= 'def' ) .or. ( b1(2)%i /= 100002 ) ) error stop 13_4
   end select

   select type ( i => b2 )
      class default
         read ( 1, "(DT(3,4),DT(3,3),DT(3,2))", iostat = stat, iomsg = msg , rec = 1 ) i
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) ) error stop 14_4
   end select

   select type ( b2 )
      type is ( child )
         if ( ( b2(1)%c /= 'jkl' ) .or. ( b2(1)%i /= 1002 ) .or. &
              ( b2(2)%c /= 'mno' ) .or. ( b2(2)%i /= 101 ) .or. &
              ( b2(3)%c /= 'pqr' ) .or. ( b2(3)%i /= 10 ) ) error stop 15_4
   end select

   close ( 1, status ='delete')

end program
