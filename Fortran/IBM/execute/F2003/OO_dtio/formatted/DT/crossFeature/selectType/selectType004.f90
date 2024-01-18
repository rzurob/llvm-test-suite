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
!*                                        Select-Type Constructor: Unlimited Polymorphic Array Entities
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

program selectType004
use m

   class(*), allocatable  :: u1(:)
   class(*), pointer      :: u2(:,:)
   
   character(81) :: fmt = "(DT'_u1-3'(7,2), DT'_u1-2'(8,3), DT'_u1-1'(9,4), DT'_u1-4'(9,3), DT'_u1-1'(7,2) )"

   open (1, file = 'selectType004.1', form='formatted', access='sequential' )

   allocate ( u1(4), source = (/ base( 101.0 ), base( 102.0 ), base( 103.0 ), base( 104.0 ) /) )
   allocate ( u2(2,2), source = reshape ( source = (/ child ( 201.0 , 211.0 ), child ( 202.0 , 212.0 ), &
                                                      child ( 203.0 , 213.0 ), child ( 204.0 , 214.0 ) /), shape = (/2,2/) ) )

   select type ( g => u1((/3,2,1,4,1/)) )
      class is ( base )
         write ( 1, fmt, iostat = stat, iomsg = msg ) g         !<= 5 elements
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4
   end select

   select type ( u2 )
      class is ( child )
         write ( 1, "(DT'_u2'(7,2,8,3))", iostat = stat, iomsg = msg ) u2
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4
   end select

   deallocate ( u1, u2 )
   allocate ( u2(2,2), source = reshape ( source = (/ child ( 301.0 , 311.0 ), child ( 302.0 , 312.0 ), &
                                                      child ( 303.0 , 313.0 ), child ( 304.0 , 314.0 ) /), shape = (/2,2/) ) )
   allocate ( u1(4), source = (/ child ( 401.0 , 411.0 ), child ( 402.0 , 412.0 ), &
                                 child ( 403.0 , 413.0 ), child ( 404.0 , 414.0 ) /) )
 
   select type ( g => (/ u1(2), u1(4) /) )
      class is ( child )
         write ( 1, "(DT'_u1-1'(7,2,8,3), DT'_u1-2'(8,3,9,4))", iostat = stat, iomsg = msg ) g  !<= two elements
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4
   end select

   select type ( g => u2(1,1:2) )
      type is ( child )
         write ( 1, "(DT'_u2'(8,3,9,4))", iostat = stat, iomsg = msg ) g  !<= two elements
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
