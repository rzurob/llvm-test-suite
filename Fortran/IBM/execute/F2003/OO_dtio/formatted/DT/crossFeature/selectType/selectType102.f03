!*  ===================================================================
!*
!*  DATE                       : 21/03/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        Select-Type Constructor: Polymorphic Array Entities (read)
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
   character(20) :: rbuffer(12)
   integer(4) :: idx

end module

program selectType102
use m

   class(base), allocatable  :: b1(:)
   class(base), pointer      :: b2(:,:)

   class(child), allocatable :: c1(:,:)
   class(child), pointer     :: c2(:)

   character(81) :: fmt = "(DT'_b1-4'(7,2), DT'_b1-3'(8,3), DT'_b1-2'(8,3), DT'_b1-1'(7,2) )"

   open (1, file = 'selectType102.1', form='formatted', access='sequential' )

   allocate ( b1(4) )
   allocate ( child :: b2(2,2) )
   allocate ( c1(2,2) )
   allocate ( c2(4) )

   idx = 1

   select type ( g => b1(4:1:-1) )
      class is ( base )
         read ( 1, fmt, iostat = stat, iomsg = msg ) g
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4
   end select

   select type ( b2 )
      class default
         read ( 1, "(DT'_b2'(7,2,8,3))", iostat = stat, iomsg = msg ) b2
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4
   end select

   select type ( g => c1(1,1:2) )
      type is ( child )
         read ( 1, "(DT'_c1'(8,3,9,4))", iostat = stat, iomsg = msg ) g  !<= two elements
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 3_4
   end select

   select type ( g =>  c2(2:4:2) )
      class default
         read ( 1, "(DT'_c2-1'(7,2,8,3), DT'_c2-2'(8,3,9,4))", iostat = stat, iomsg = msg ) g  !<= two elements
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 4_4
   end select

   print *, b1%i
   select type ( b2 )
      type is ( child )
         print *, b2%i
         print *, b2%j
   end select

   print *, c1%i
   print *, c1%j
   print *, c2%i
   print *, c2%j

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