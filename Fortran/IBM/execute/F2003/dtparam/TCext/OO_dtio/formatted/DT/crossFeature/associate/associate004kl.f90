!*  ===================================================================
!*
!*  TEST CASE NAME             : associate004kl
!*
!*  DATE                       : 2007-06-06 (original: 21/03/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        Associate Constructor: Unlimited Polymorphic Array Entities
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

program associate004kl
use m

   class(*), pointer         :: u1(:)
   class(*), allocatable     :: u2(:,:)

   class(base(4)), allocatable  :: b1(:)
   class(base(4)), pointer      :: b2(:,:)

   open (1, file = 'associate004kl.1', form='formatted', access='direct', recl=200 )
   allocate ( b1(3), source = (/ base(4)(111.0), base(4)(112.0), base(4)(113.0) /)  )
   allocate ( b2(2,2) , source = reshape( source = (/ child(4,4) ( 121.0 , 122.0 ), child(4,4) ( 123.0 , 124.0 ), &
                                                     child(4,4) ( 125.0 , 126.0 ), child(4,4) ( 127.0 , 128.0 )/), shape = (/2,2/) ) )
   allocate ( u1(3), source = b1 )
   allocate ( u2(2,2), source = b2 )

   associate ( g => u1((/1,2,3/)) )
      select type ( g )
         class is ( base(4) )
            write ( 1, "(DT'b1-1'(7,2), DT'b1-2'(7,2), DT'b1-3'(7,2))", iostat = stat, iomsg = msg, rec = 5 ) g
            if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4
      end select
   end associate

   select type ( u2 )
      class is ( base(4) )
         associate ( g => u2(1:2,1:2) )
            write ( 1, "(DT'u2-1'(7,2,8,3),DT'u2-2'(7,2,8,3))", iostat = stat, iomsg = msg, rec = 3) g
            if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4
         end associate
   end select

   deallocate ( u1, u2 )

   allocate ( u1(2), source = (/ child(4,4)(201.0, 202.0), child(4,4)(203.0, 204.0 ) /) )
   allocate ( u2(1,1), source = reshape ( source =  (/ base(4) (205.0) /), shape = (/1,1 /) ) )

   associate ( d => u1 )
      select type ( d )
         type is ( child(4,4) )
            write ( 1, "(DT'u1-1'(8,3,9,4), DT'u1-2'(8,3,9,4))", iostat = stat, iomsg = msg, rec = 2 ) d
            if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4
      end select
   end associate

   select type ( f => u2 )
      class is ( base(4) )
         associate ( g => f )
            write ( 1, "(DT(7,2))", iostat = stat, iomsg = msg, rec = 1 ) g
            if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 4_4
         end associate
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
