!*  ===================================================================
!*
!*  DATE                       : 21/03/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        Associate Constructor: Unlimited Polymorphic Scalar Entities
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

   interface write(formatted)
      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   integer :: stat
   character(150) :: msg

end module

program associate003
use m

   class(*), allocatable  :: u1
   class(*), pointer      :: u2

   class(base), allocatable  :: b1
   class(child), allocatable :: c1

   open (1, file = 'associate003.1', form='formatted', access='stream' )

   allocate ( b1, source = base  ( 101.0 ) )
   allocate ( c1, source = child ( 102.0 , 103.0 ) )

   allocate ( u1, source = b1 )
   allocate ( u2, source = c1 )

   associate ( g => u1 )
      select type ( g )
         class is ( base )
            write ( 1, "(DT(7,2))", iostat = stat, iomsg = msg, pos = 1 ) g
            if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4
      end select
   end associate

   select type ( u2 )
      class is ( base )
         associate ( g => u2 )
            write ( 1, "(DT(7,2,8,3))", iostat = stat, iomsg = msg, pos = 50 ) g
            if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4
         end associate
   end select

   deallocate ( u1, u2 )

   allocate ( u1, source = child(201.0, 202.0) )
   allocate ( u2, source = base (203.0) )

   associate ( d => u1 )
      select type ( d )
         type is ( child )
            write ( 1, "(DT(8,3,9,4))", iostat = stat, iomsg = msg, pos = 100 ) d
            if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4
      end select
   end associate

   select type ( f => u2 )
      class is ( base )
         associate ( g => f )
            write ( 1, "(DT(7,2))", iostat = stat, iomsg = msg, pos = 150 ) g
            if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 4_4
         end associate
   end select

end program

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child

   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(25) :: fmt

   write ( unit, * ) ' iotype:', iotype, ' v_list:', v_list

   select type ( dtv )
      type is ( base )
         write ( fmt, "(A2,I1,A1,I1,A1)" ) '(F', v_list(1),'.',v_list(2),')'
         write ( unit, fmt, iostat = iostat )    dtv%i
      type is ( child )
         write ( fmt, "(A2,I1,A1,I1,A2,I1,A1,I1,A1)" ) '(F', v_list(1),'.',v_list(2),',F',v_list(3),'.',v_list(4),')'
         write ( unit, fmt, iostat = iostat )    dtv%i, dtv%j
   end select
   iomsg = 'dtiowrite'

end subroutine
