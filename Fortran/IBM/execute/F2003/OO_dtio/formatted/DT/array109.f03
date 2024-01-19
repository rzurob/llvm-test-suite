!*  ===================================================================
!*
!*  DATE                       : 21/03/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        Array unlimited polymorphic entity contains
!*                                        unlimited polymorphic component within select type (read)
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
      integer(4) :: i
      class(*), pointer :: u => null()
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
   character(15) :: rbuffer(6)

end module

program array109
use m

   class(*), target, allocatable :: u1(:)
   class(*), pointer             :: u2

   integer, target :: i1 = -99
   character(3), target :: c1 = 'xxx'

   integer :: stat
   character(150) :: msg
   character(30) :: fmt = "(DT'_u1'(1))"

   open (1, file = 'array109.1', form='formatted', access='sequential' )

   allocate( base :: u1(3) )
   allocate( base :: u2 )

10 format (DT'_u2'(2,3))
   idx = 1
   select type ( u1 )
      type is ( base )

         u1(1)%u => i1
         u1(2)%u => c1
         u1(3)%u => u2

         read ( 1, "(DT'_u1-1'(1),DT'_u1-2'(-2),DT'_u1-3'(3))", iostat = stat, iomsg = msg )   u1
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )        error stop 1_4

         select type ( g => u1(1)%u )
            type is (integer)
               if ( ( g /= 555 )   .or. ( u1(1)%i /= 101 ) )    error stop 2_4
         end select

         select type ( g => u1(2)%u )
            type is (character(*))
               if ( ( g /= 'IBM' ) .or. ( u1(2)%i /= 102 ) )    error stop 3_4
         end select

         select type ( g => u1(3)%u )
            type is (base)
               if ( ( g%i /= 201 ) .or. ( u1(3)%i /= 103 ) )    error stop 4_4
         end select

         u1(2)%u => null()

         read ( 1, "(2(DT'_u1'(-1,-2,-3)), DT)", iostat = stat, iomsg = msg )                  u1
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )        error stop 5_4

         select type ( g => u1(1)%u )
            type is (integer)
               if ( ( g /= 777 )   .or. ( u1(1)%i /= 104 ) )    error stop 6_4
         end select

         if (( u1(2)%i /= 105 ) )          error stop 7_4

         select type ( g => u1(1)%u )
            type is (base)
               if ( ( g%i /= 201 ) .or. ( u1(3)%i /= 106 ) )    error stop 8_4
         end select

   end select

   print *, rbuffer

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, rbuffer, idx

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write ( rbuffer(idx), * ) iotype, v_list
   idx = idx + 1

   if ( associated(dtv%u) ) then
      select type ( g => dtv%u )
         type is ( integer )
            read ( unit, "(/I4)" ) g
         type is ( character(*) )
            read ( unit, "(/1X,A3)" ) g
         type is ( base )
            read ( unit, "(/I4)" ) g%i
      end select
   end if

   read ( unit, "(/I4)" ) dtv%i

   iomsg = 'dtioread'

end subroutine
