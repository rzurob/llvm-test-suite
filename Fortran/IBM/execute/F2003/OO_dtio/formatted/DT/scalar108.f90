!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 21/03/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        scalar unlimited polymorphic entity within select type
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
      character(3) :: c
   end type

   type, extends(base) :: child
      integer(4) :: i
   end type

   type, extends(child) :: gen3
      integer(4) :: j
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

   integer :: idx
   character(20) :: rbuffer(4)

end module

program scalar108
use m

   class(*), target, allocatable :: u1, u2
   class(*), pointer             :: u3, u4

   integer :: stat
   character(150) :: msg
   character(30) :: fmt = "(DT'_u1'(1))"

   open (1, file = 'scalar108.1', form='formatted', access='sequential' )

   allocate( u1, source = base('xxx'))
   allocate( u2, source = child('xxx',-999))
   allocate( u3, source = gen3('xxx',-999,-999))
   u4 => u1

10 format (DT'_u2'(2,3))
20 format (DT'_u3'(4,5,6))

   idx = 1
   select type ( u1 )
      type is ( base )
         read ( 1, fmt, iostat = stat, iomsg = msg )                  u1
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )   error stop 1_4
         if ( u1%c /= 'abc' ) error stop 2_4
   end select

   select type ( u2 )
      class is ( base )
         read ( 1, 10, iostat = stat, iomsg = msg )                   u2
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )   error stop 3_4
         select type ( u2 )
            type is (child)
               if ( ( u2%c /= 'def' ) .or. ( u2%i /= 123 ) )     error stop 4_4
         end select
   end select

   select type ( u3 )
      class is ( child )
         read ( 1, 20, iostat = stat, iomsg = msg )                   u3
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )   error stop 5_4
         select type ( u3 )
            type is (gen3)
               if ( ( u3%c /= 'ghi' ) .or. ( u3%i /= 456 ) .or. ( u3%j /= 789 ) )     error stop 6_4
         end select
   end select

   select type ( u4 )
      type is ( base )
         read ( 1, "(DT'_u4'(7))", iostat = stat, iomsg = msg )       u4
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )   error stop 7_4
         if ( u4%c /= 'jkl' ) error stop 8_4
         select type ( u3 )
            class is ( base )
               u4 = u3
               read ( 1, "(DT'_u4'(8,9,10))", iostat = stat, iomsg = msg ) u4
               if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 9_4
               if ( u4%c /= 'mno' ) error stop 10_4
         end select
   end select

   print *, rbuffer

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child, gen3, rbuffer, idx

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write ( rbuffer(idx), * ) iotype, v_list
   idx = idx + 1

   select type ( dtv )
      class is ( base )
         read ( unit, "(/5X,A3)" ) dtv%c
   end select
   select type ( dtv )
      class is ( child )
         read ( unit, "(/5X,I3)" ) dtv%i
   end select
   select type ( dtv )
      class is ( gen3 )
         read ( unit, "(/5X,I3)" ) dtv%j
   end select

   iomsg = 'dtioread'

end subroutine
