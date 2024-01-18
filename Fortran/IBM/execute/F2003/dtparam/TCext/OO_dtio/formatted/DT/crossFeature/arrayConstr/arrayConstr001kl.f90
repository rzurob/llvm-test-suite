!*  ===================================================================
!*
!*  TEST CASE NAME             : arrayConstr001kl
!*
!*  DATE                       : 2007-06-06 (original: 21/03/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        Array Constructor containing structure constructor
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
      integer(kb) :: i
   end type

   type, extends(base) :: child (lc)
      integer, len :: lc
      character(lc) :: c = 'xxx'
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

program arrayConstr001kl
use m

   open (1, file = 'arrayConstr001kl.1', form='formatted', access='sequential' )

   write ( 1, "(DT'_base-1'(4),/,DT'_base-2'(5),/,DT'_base-3'(6))", iostat = stat, iomsg = msg )   (/ base(4)(101), base(4)(102), base(4)(103) /)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   write ( 1, "(DT'_child-1'(4,4),/,DT'_child-2'(5,5),/,DT'_child-3'(6,6))", iostat = stat, iomsg = msg )   (/ child(4,3)(201,'ABC'), child(4,3)(202,'DEF'), child(4,3)(203,'GHI') /)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

   write ( 1, "(DT'_base-1'(4))", iostat = stat, iomsg = msg )   (/ ( base(4)(i+300), i = 1, 3 ) /)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4

   write ( 1, "(DT'_child-1'(5,5),DT'_child-2'(6,6),)", iostat = stat, iomsg = msg )   (/ ( child(4,3)(i+300,'IBM'), i = 101, 103 ) /)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 4_4

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
         write ( fmt, "(A2,I1,A1)" ) '(I', v_list(1),')'
         write ( unit, fmt, iostat = iostat )    dtv%i
      type is ( child(4,*) )
         write ( fmt, "(A2,I1,A2,I1,A1)" ) '(I', v_list(1),',A',v_list(2),')'
         write ( unit, fmt, iostat = iostat )    dtv%i, dtv%c
   end select
   iomsg = 'dtiowrite'

end subroutine
