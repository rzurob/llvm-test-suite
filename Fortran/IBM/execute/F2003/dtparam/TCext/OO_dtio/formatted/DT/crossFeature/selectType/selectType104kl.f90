!*  ===================================================================
!*
!*  TEST CASE NAME             : selectType104kl
!*
!*  DATE                       : 2007-06-06 (original: 21/03/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        Select-Type Constructor: Unlimited Polymorphic Array Entities (read)
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
   character(20) :: rbuffer(16)
   integer(4) :: idx

end module

program selectType104kl
use m

   class(*), allocatable  :: u1(:)
   class(*), pointer      :: u2(:,:)

   character(81) :: fmt = "(DT'_u1-4'(7,2), DT'_u1-3'(8,3), DT'_u1-2'(9,4), DT'_u1-1'(9,3) )"

   rbuffer = ''

   open (1, file = 'selectType104kl.1', form='formatted', access='sequential' )
   idx = 1
   allocate ( base(4) :: u1(4) )
   allocate ( child(4,4) :: u2(2,2) )

   select type ( g => u1(4:1:-1) )
      class is ( base(4) )
         read ( 1, fmt, iostat = stat, iomsg = msg ) g
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4
         select type ( u1 )
            type is ( base(4) )
               print *, u1%i
         end select
   end select

   select type ( u2 )
      class is ( child(4,4) )
         read ( 1, "(DT'_u2'(7,2,8,3))", iostat = stat, iomsg = msg ) u2
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4
         print *, u2%i
         print *, u2%j
   end select

   deallocate ( u1, u2 )
   allocate ( child(4,4) :: u2(2,2) )
   allocate ( child(4,4) :: u1(4) )

   select type ( g => u1(2:4:2) )
      class is ( child(4,4) )
         read ( 1, "(DT'_u1-1'(7,2,8,3), DT'_u1-2'(8,3,9,4))", iostat = stat, iomsg = msg ) g  !<= two elements
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 3_4
         select type ( u1 )
            type is ( child(4,4) )
               print *, u1%i
               print *, u1%j
         end select
   end select

   select type ( g => u2(1,1:2) )
      type is ( child(4,4) )
         read ( 1, "(DT'_u2'(8,3,9,4))", iostat = stat, iomsg = msg ) g  !<= two elements
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 4_4
         select type ( u2 )
            type is ( child(4,4) )
               print *, u2%i
               print *, u2%j
         end select
   end select

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
