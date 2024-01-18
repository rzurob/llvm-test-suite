!*  ===================================================================
!*
!*  TEST CASE NAME             : associate002kl
!*
!*  DATE                       : 2007-06-06 (original: 21/03/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        Associate Constructor: (Non-) Polymorphic Array Entities
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

program associate002kl
use m

   class(base(4)), allocatable  :: b1(:)
   class(base(4)), pointer      :: b2(:,:)
   type(base(4))                :: b3(2,2) = reshape( source = (/ base(4)(131.0), base(4)(132.0) , base(4)(133.0), base(4)(134.0) /), shape = (/2,2/) )

   type(child(4,4)), allocatable  :: c1(:)
   class(child(4,4)), pointer     :: c2(:)
   type(child(4,4))               :: c3(3) = (/ child(4,4)(231.0, 232.0), child(4,4)(233.0, 234.0), child(4,4)(235.0, 236.0) /)

   open (1, file = 'associate002kl.1', form='formatted', access='direct', recl=200 )

   allocate ( b1(3), source = (/ base(4)(111.0), base(4)(112.0), base(4)(113.0) /)  )
   allocate ( b2(2,2) , source = reshape( source = (/ child(4,4) ( 121.0 , 122.0 ), child(4,4) ( 123.0 , 124.0 ), &
                                                     child(4,4) ( 125.0 , 126.0 ), child(4,4) ( 127.0 , 128.0 )/), shape = (/2,2/) ) )
   allocate ( c1(2), source = (/ child(4,4) ( 211.0 , 212.0 ), child(4,4)(213.0, 214.0) /) )
   allocate ( c2(3), source = (/ child(4,4) ( 221.0 , 222.0 ), child(4,4) ( 223.0 , 224.0 ), child(4,4) ( 225.0 , 226.0 ) /) )

   associate ( g => b1 )
      write ( 1, "(DT'b1-1'(7,2),DT'b1-2'(7,2), DT'b1-3'(7,2))", iostat = stat, iomsg = msg, rec = 7)           g
      if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4
   end associate

   associate ( g => b2 )
      write ( 1, "(DT'b2-1'(7,2,8,3), DT'b2-2'(7,2,8,3))", iostat = stat, iomsg = msg, rec = 5 ) g
      if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4
   end associate

   associate ( g => b3 )
      write ( 1, "(DT'b3-1'(8,3), DT'b3-2'(8,3), DT'b3-3'(8,3), DT'b3-4'(8,3))", iostat = stat, iomsg = msg, rec = 4 )     g
      if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4
   end associate

   associate ( g => c1 )
      write ( 1, "(DT'c1-1'(8,3,9,4),DT'c1-2'(8,3,9,4))", iostat = stat, iomsg = msg, rec = 3 ) g
      if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 4_4
   end associate

   associate ( g => c2 )
      write ( 1, "(DT'c2-1'(7,2,9,4),DT'c2-2'(7,2,9,4),DT'c2-3'(7,2,9,4))", iostat = stat, iomsg = msg, rec = 2 ) g
      if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 5_4
   end associate

   associate ( g => c3 )
      write ( 1, "(DT'c3-1'(9,3,9,4),DT'c3-2'(9,3,9,4),DT'c3-3'(9,3,9,4))", iostat = stat, iomsg = msg, rec = 1 ) g
      if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 6_4
   end associate

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
