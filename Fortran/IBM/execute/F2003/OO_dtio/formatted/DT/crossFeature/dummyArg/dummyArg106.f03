!*  ===================================================================
!*
!*  DATE                       : 21/03/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        Argument Association: scalar unlimited
!*                                        polymorphic dummy argument (read)
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
      integer(4) :: i = -999
   end type

   type, extends(base) :: child
      integer(4) :: j = -999
   end type

   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   integer :: stat
   character(150) :: msg
   character(20) :: rbuffer(10)
   integer :: idx

   contains

      subroutine foo ( dtv )
         class(*), intent(inout) :: dtv
         character(51) :: fmt = ''
         fmt = "(DT'_foo1'(5,6))"
         select type ( dtv )
            class is ( base )
               read (1, fmt, iostat = stat, iomsg = msg ) dtv
         end select
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )  error stop 1_4

      end subroutine

      subroutine bar ( dtv )
         class(*), allocatable, intent(inout) :: dtv
      10 format (DT'_bar1'(4,5))

         select type ( dtv )
            type is ( base )
               read (1, 10, iostat = stat, iomsg = msg ) dtv
            type is ( child )
               read (1, "(DT'_bar4'(5,6))", iostat = stat, iomsg = msg ) dtv
         end select

         if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )  error stop 2_4

      end subroutine

      subroutine boo ( dtv )
         class(*), pointer, intent(inout) :: dtv
      10 format (DT'_boo1'(4,5))

         select type ( g => dtv )
            class is ( base )
               read (1, 10, iostat = stat, iomsg = msg ) g
         end select

         if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )  error stop 3_4

      end subroutine

end module

program dummyArg106
use m

   type(base), allocatable  :: b1
   type(base)               :: b2
   class(base), pointer     :: b3

   type(child), allocatable :: c1
   type(child)              :: c2
   class(child), pointer    :: c3

   class(*), allocatable :: u1
   class(*), pointer     :: u2

   open (1, file = 'dummyArg106.1', form='formatted', access='sequential' )

   allocate ( b1 )
   allocate ( child :: b3 )

   allocate ( c1 )
   allocate ( c3 )

   idx = 1

   call foo ( b1 )
   call foo ( b2 )
   call foo ( b3 )
   call foo ( c1 )
   call foo ( c2 )
   call foo ( c3 )

   if ( b1%i /=  101 ) error stop 4_4
   if ( b2%i /=  111 ) error stop 5_4

   select type ( b3 )
      type is ( child )
         if ( ( b3%i /=  121 ) .or. ( b3%j /= 122 ) ) error stop 6_4
   end select

   if ( ( c1%i /=  201 ) .or. ( c1%j /= 202 ) ) error stop 7_4
   if ( ( c2%i /=  211 ) .or. ( c2%j /= 212 ) ) error stop 8_4
   if ( ( c3%i /=  221 ) .or. ( c3%j /= 222 ) ) error stop 9_4

   allocate ( u1, source = b1 )

   call foo ( u1 )

   select type ( u1 )
      type is ( base )
         if ( u1%i /=  104 ) error stop 10_4
   end select

   call bar ( u1 )

   select type ( u1 )
      type is ( base )
         if ( u1%i /=  105 ) error stop 11_4
   end select

   deallocate ( u1 )

   allocate ( u1, source = c3 )
   call bar ( u1 )
   select type ( u1 )
      type is ( child )
         if ( ( u1%i /=  223 ) .or. ( u1%j /= 224 ) ) error stop 12_4
   end select
   allocate ( u2, source = c1 )
   call boo ( u2 )

   select type ( u2 )
      type is ( child )
         if ( ( u2%i /=  203 ) .or. ( u2%j /= 204 ) ) error stop 13_4
   end select

   print *, rbuffer

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child, rbuffer, idx

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(25) :: fmt

   write ( rbuffer(idx), * ) iotype, v_list
   idx = idx + 1

   select type ( dtv )
      type is ( base )
         write ( fmt, * ) '(I', v_list(1),')'
         read ( unit, fmt, iostat = iostat )    dtv%i
      type is ( child )
         write ( fmt, * ) '(I', v_list(1),', I', v_list(2),')'
         read ( unit, fmt, iostat = iostat )    dtv%i, dtv%j
   end select
   iomsg = 'dtioread'

end subroutine
