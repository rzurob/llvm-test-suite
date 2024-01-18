!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : associate101kl
!*
!*  PROGRAMMER                 : David Forster (derived from associate101 by Robert Ma)
!*  DATE                       : 2007-06-06 (original: 21/03/2005)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        Associate Constructor: (Non-) Polymorphic Scalar Entities (read)
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
   character(20) :: rbuffer(6)
   integer(4) :: idx

end module

program associate101kl
use m

   class(base(4)), allocatable  :: b1
   class(base(4)), pointer      :: b2
   type(base(4))                :: b3 = base(4) ( 104.0 )

   type(child(4,4)), allocatable  :: c1
   class(child(4,4)), pointer     :: c2
   type(child(4,4))               :: c3 = child(4,4) ( 205.0, 206.0 )
   logical :: precision_r4

   open (1, file = 'associate101kl.1', form='formatted', access='sequential' )

   allocate ( b1 )
   allocate ( child(4,4) :: b2 )
   allocate ( c1, c2 )

   idx = 1

   associate ( g => b1 )
      read ( 1, "(DT(7,2))", iostat = stat, iomsg = msg ) g
      if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4
   end associate

   associate ( g => b2 )
      read ( 1, "(DT'_b2'(7,2,8,3))", iostat = stat, iomsg = msg ) g
      if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4
   end associate

   associate ( g => b3 )
      read ( 1, "(DT'_b3'(8,3))", iostat = stat, iomsg = msg ) g
      if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 3_4
   end associate

   associate ( g => c1 )
      read ( 1, "(DT'_c1'(8,3,9,4))", iostat = stat, iomsg = msg ) g
      if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 4_4
   end associate

   associate ( g => c2 )
      read ( 1, "(DT'_c2'(7,2,9,4))", iostat = stat, iomsg = msg ) g
      if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 5_4
   end associate

   associate ( g => c3 )
      read ( 1, "(DT'_c3'(9,3,9,4))", iostat = stat, iomsg = msg ) g
      if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 6_4
   end associate


   if ( .not. precision_r4( b1%i , 101.0 ) ) error stop 7_4

   select type ( b2 )
      type is ( child(4,4) )
         if ( ( .not. precision_r4( b2%i , 102.0 ) ) .or. ( .not. precision_r4( b2%j , 103.0 ) ) ) error stop 8_4
   end select

   if ( .not. precision_r4( b3%i , 104.0 ) ) error stop 9_4

   if ( ( .not. precision_r4( c1%i , 201.0 ) ) .or. ( .not. precision_r4( c1%j , 202.0 ) ) ) error stop 10_4
   if ( ( .not. precision_r4( c2%i , 203.0 ) ) .or. ( .not. precision_r4( c2%j , 204.0 ) ) ) error stop 11_4
   if ( ( .not. precision_r4( c3%i , 205.0 ) ) .or. ( .not. precision_r4( c3%j , 206.0 ) ) ) error stop 12_4

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
