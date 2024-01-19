!*  ===================================================================
!*
!*  DATE                       : 21/03/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        Select-Type Constructor: Polymorphic Scalar Entities (read)
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
      real(4) :: i = -999.0
   end type

   type, extends(base) :: child
      real(4) :: j = -999.0
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
   character(20) :: rbuffer(4)
   integer(4) :: idx

end module

program selectType101
use m

   class(base), allocatable  :: b1
   class(base), pointer      :: b2

   class(child), allocatable :: c1
   class(child), pointer     :: c2

   logical :: precision_r4

   open (1, file = 'selectType101.1', form='formatted', access='sequential' )

   allocate ( b1 )
   allocate ( b2, source = child ( 1.0 , 2.0 ) )
   allocate ( c1 )
   allocate ( c2 )

   idx = 1

   select type ( g => b1 )
      class is ( base )
         read ( 1, "(DT(7,2))", iostat = stat, iomsg = msg ) g
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4
   end select

   select type (  b2 )
      class default
         read ( 1, "(DT'_b2'(7,2,8,3))", iostat = stat, iomsg = msg ) b2
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4
   end select

   select type ( g => c1 )
      type is ( child )
         read ( 1, "(DT'_c1'(8,3,9,4))", iostat = stat, iomsg = msg ) g
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 3_4
   end select

   select type ( c2 )
      class default
         read ( 1, "(DT'_c2'(7,2,9,4))", iostat = stat, iomsg = msg ) c2
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 4_4
   end select

   if ( .not. precision_r4(b1%i,101.0 ) ) error stop 5_4
   select type ( b2 )
      type is ( child )
         if ( ( .not. precision_r4(b2%i,102.0 ) ) .or. ( .not. precision_r4(b2%j,103.0 ) ) ) error stop 6_4
   end select

   if ( ( .not. precision_r4(c1%i,201.0 ) ) .or. ( .not. precision_r4(c1%j,202.0 ) ) )      error stop 7_4
   if ( ( .not. precision_r4(c2%i,203.0 ) ) .or. ( .not. precision_r4(c2%j,204.0 ) ) )      error stop 8_4


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
         write ( fmt, "(A2,I1,A1,I1,A1)" ) '(F', v_list(1),'.',v_list(2),')'
         read ( unit, fmt, iostat = iostat )    dtv%i
      type is ( child )
         write ( fmt, "(A2,I1,A1,I1,A2,I1,A1,I1,A1)" ) '(F', v_list(1),'.',v_list(2),',F',v_list(3),'.',v_list(4),')'
         read ( unit, fmt, iostat = iostat )    dtv%i, dtv%j
   end select

   iomsg = 'dtioread'

end subroutine
