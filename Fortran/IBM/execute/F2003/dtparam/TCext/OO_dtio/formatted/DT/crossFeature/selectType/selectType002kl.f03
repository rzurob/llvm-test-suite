!*  ===================================================================
!*
!*  DATE                       : 2007-06-06 (original: 21/03/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        Select-Type Constructor: Polymorphic Array Entities
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

   type base (kb)
      integer, kind :: kb
      real(kb) :: i = -999.0
   end type

   type, extends(base) :: child (kc)
      integer, kind :: kc
      real(kc) :: j = -999.0
   end type

   interface write(formatted)
      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(4)), intent(in) :: dtv
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

program selectType002kl
use m

   class(base(4)), allocatable  :: b1(:)
   class(base(4)), pointer      :: b2(:,:)

   class(child(4,4)), allocatable :: c1(:,:)
   class(child(4,4)), pointer     :: c2(:)

   character(81) :: fmt = "(DT'_b1-3'(7,2), DT'_b1-2'(8,3), DT'_b1-1'(9,4), DT'_b1-4'(9,3), DT'_b1-1'(7,2) )"

   open (1, file = 'selectType002kl.1', form='formatted', access='sequential' )

   allocate ( b1(4), source = (/ base(4)( 101.0 ), base(4)( 102.0 ), base(4)( 103.0 ), base(4)( 104.0 ) /) )
   allocate ( b2(2,2), source = reshape ( source = (/ child(4,4) ( 201.0 , 211.0 ), child(4,4) ( 202.0 , 212.0 ), &
                                                      child(4,4) ( 203.0 , 213.0 ), child(4,4) ( 204.0 , 214.0 ) /), shape = (/2,2/) ) )
   allocate ( c1(2,2), source = reshape ( source = (/ child(4,4) ( 301.0 , 311.0 ), child(4,4) ( 302.0 , 312.0 ), &
                                                      child(4,4) ( 303.0 , 313.0 ), child(4,4) ( 304.0 , 314.0 ) /), shape = (/2,2/) ) )
   allocate ( c2(4), source = (/ child(4,4) ( 401.0 , 411.0 ), child(4,4) ( 402.0 , 412.0 ), &
                                 child(4,4) ( 403.0 , 413.0 ), child(4,4) ( 404.0 , 414.0 ) /) )

   select type ( g => b1((/3,2,1,4,1/)) )
      class is ( base(4) )
         write ( 1, fmt, iostat = stat, iomsg = msg ) g         !<= 5 elements
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4
   end select

   select type ( b2 )
      class default
         write ( 1, "(DT'_b2'(7,2,8,3))", iostat = stat, iomsg = msg ) b2
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4
   end select

   select type ( g => c1(1,1:2) )
      type is ( child(4,4) )
         write ( 1, "(DT'_c1'(8,3,9,4))", iostat = stat, iomsg = msg ) g  !<= two elements
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4
   end select

   select type ( g => (/ c2(2), c2(4) /) )
      class default
         write ( 1, "(DT'_c2-1'(7,2,8,3), DT'_c2-2'(8,3,9,4))", iostat = stat, iomsg = msg ) g  !<= two elements
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 4_4
   end select

end program

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child

   class(base(4)), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(25) :: fmt

   write ( unit, * ) ' iotype:', iotype, ' v_list:', v_list

   select type ( dtv )
      type is ( base(4) )
         write ( fmt, "(A2,I1,A1,I1,A1)" ) '(F', v_list(1),'.',v_list(2),')'
         write ( unit, fmt, iostat = iostat )    dtv%i
      type is ( child(4,4) )
         write ( fmt, "(A2,I1,A1,I1,A2,I1,A1,I1,A1)" ) '(F', v_list(1),'.',v_list(2),',F',v_list(3),'.',v_list(4),')'
         write ( unit, fmt, iostat = iostat )    dtv%i, dtv%j
   end select
   iomsg = 'dtiowrite'

end subroutine
