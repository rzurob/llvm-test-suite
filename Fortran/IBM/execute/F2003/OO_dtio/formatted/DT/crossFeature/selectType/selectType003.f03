!*  ===================================================================
!*
!*  DATE                       : 21/03/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        Select-Type Constructor: Unlimited Polymorphic Scalar Entities
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

program selectType003
use m

   class(*), allocatable     :: u1
   class(*), pointer         :: u2

   class(base), allocatable  :: b1
   class(base), pointer      :: b2

   class(child), allocatable :: c1
   class(child), pointer     :: c2

   open (1, file = 'selectType003.1', form='formatted', access='sequential' )

   allocate ( b1, source = base  ( 101.0 ) )
   allocate ( b2, source = child ( 102.0 , 103.0 ) )
   allocate ( c1, source = child ( 201.0 , 202.0 ) )
   allocate ( c2, source = child ( 203.0 , 204.0 ) )

   allocate ( u1, source = b1 )
   u2 => b2

   select type ( g => u1 )
      type is ( base )
         write ( 1, "(DT(7,2))", iostat = stat, iomsg = msg ) g
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4
   end select

   select type ( u2 )
      class is ( base )
         write ( 1, "(DT'_u2'(7,2,8,3))", iostat = stat, iomsg = msg ) u2
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4
   end select

   deallocate ( u1 )
   allocate ( u1, source = c1 )
   u2 => c2

   select type ( g => u1 )
      type is ( child )
         write ( 1, "(DT'_u1'(8,3,9,4))", iostat = stat, iomsg = msg ) g
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4
   end select

   select type ( u2 )
      class is ( child )
         write ( 1, "(DT'_u2'(7,2,9,4))", iostat = stat, iomsg = msg ) u2
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 4_4
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
