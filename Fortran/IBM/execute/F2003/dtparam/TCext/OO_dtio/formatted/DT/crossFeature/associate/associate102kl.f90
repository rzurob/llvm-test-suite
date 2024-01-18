!*  ===================================================================
!*
!*  TEST CASE NAME             : associate102kl
!*
!*  DATE                       : 2007-06-06 (original: 21/03/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        Associate Constructor: (Non-) Polymorphic Array Entities (read)
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

   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(4)), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   integer :: stat
   character(150) :: msg
   character(20) :: rbuffer(19)
   integer(4) :: idx

end module

program associate102kl
use m

   class(base(4)), allocatable  :: b1(:)
   class(base(4)), pointer      :: b2(:,:)
   type(base(4))                :: b3(2,2)

   type(child(4,4)), allocatable  :: c1(:)
   class(child(4,4)), pointer     :: c2(:)
   type(child(4,4))               :: c3(3)

   open (1, file = 'associate102kl.1', form='formatted', access='direct', recl=60 )

   allocate ( b1(3)  )
   allocate ( child(4,4) :: b2(2,2) )
   allocate ( c1(2) )
   allocate ( c2(3) )

   idx = 1

   associate ( g => b1 )
      read ( 1, "(DT'b1-1'(7,2),DT'b1-2'(7,2), DT'b1-3'(7,2))", iostat = stat, iomsg = msg, rec = 7)           g
      if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4
   end associate

   associate ( g => b2 )
      read ( 1, "(DT'b2-1'(7,2,8,3), DT'b2-2'(7,2,8,3))", iostat = stat, iomsg = msg, rec = 5 ) g
      if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4
   end associate

   associate ( g => b3 )
      read ( 1, "(DT'b3-1'(8,3), DT'b3-2'(8,3), DT'b3-3'(8,3), DT'b3-4'(8,3))", iostat = stat, iomsg = msg, rec = 4 )     g
      if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 3_4
   end associate

   associate ( g => c1 )
      read ( 1, "(DT'c1-1'(8,3,9,4),DT'c1-2'(8,3,9,4))", iostat = stat, iomsg = msg, rec = 3 ) g
      if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 4_4
   end associate

   associate ( g => c2 )
      read ( 1, "(DT'c2-1'(7,2,9,4),DT'c2-2'(7,2,9,4),DT'c2-3'(7,2,9,4))", iostat = stat, iomsg = msg, rec = 2 ) g
      if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 5_4
   end associate

   associate ( g => c3 )
      read ( 1, "(DT'c3-1'(9,3,9,4),DT'c3-2'(9,3,9,4),DT'c3-3'(9,3,9,4))", iostat = stat, iomsg = msg, rec = 1 ) g
      if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 6_4
   end associate


   print *, b1%i
   select type ( b2 )
      type is ( child(4,4) )
         print *, b2%i
         print *, b2%j
   end select

   print *, b3%i

   print *, c1%i
   print *, c1%j
   print *, c2%i
   print *, c2%j
   print *, c3%i
   print *, c3%j

   print *, rbuffer
end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child, rbuffer, idx

   class(base(4)), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(25) :: fmt

   write ( rbuffer(idx), * ) iotype, v_list
   idx = idx + 1

   select type ( dtv )
      type is ( base(4) )
         write ( fmt, "(A2,I1,A1,I1,A1)" ) '(F', v_list(1),'.',v_list(2),')'
         read ( unit, fmt, iostat = iostat )    dtv%i
      type is ( child(4,4) )
         write ( fmt, "(A2,I1,A1,I1,A2,I1,A1,I1,A1)" ) '(F', v_list(1),'.',v_list(2),',F',v_list(3),'.',v_list(4),')'
         read ( unit, fmt, iostat = iostat )    dtv%i, dtv%j
   end select
   iomsg = 'dtioread'

end subroutine
