!*  ===================================================================
!*
!*  DATE                       : 21/03/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        Associate Constructor: Unlimited Polymorphic Scalar Entities (read)
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

   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   integer :: stat
   character(150) :: msg
   character(20) :: rbuffer(4)
   integer(4) :: idx

end module

program associate103
use m

   class(*), allocatable  :: u1
   class(*), pointer      :: u2

   class(base), allocatable  :: b1
   class(child), allocatable, target :: c1

   logical :: precision_r4

   open (1, file = 'associate103.1', form='formatted', access='stream' )

   allocate ( b1, source = base  () )
   allocate ( c1, source = child () )

   allocate ( u1, source = b1 )
   u2 => c1

   idx = 1

   associate ( g => u1 )
      select type ( g )
         class is ( base )
            read ( 1, "(DT(7,2))", iostat = stat, iomsg = msg, pos = 1 ) g
            if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4
            if ( .not. precision_r4( g%i, 101.0 ) )         error stop 2_4
         class default
            error stop 3_4
      end select
   end associate

   select type ( u2 )
      class is ( base )
         associate ( g => u2 )
            read ( 1, "(DT'u2'(7,2,8,3))", iostat = stat, iomsg = msg, pos = 8 ) g
            if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )       error stop 4_4
            select type ( g )
               type is ( child )
                  if ( ( .not. precision_r4( g%i, 102.0 ) )  .or. &
                       ( .not. precision_r4( g%j, 103.0 ) )  .or. &
                       ( .not. precision_r4( c1%i, 102.0 ) ) .or. &
                       ( .not. precision_r4( c1%j, 103.0 ) ))     error stop 5_4
               class default
                  error stop 6_4
            end select
         end associate
   end select

   deallocate ( u1 )

   allocate ( u1, source = child(201.0, 202.0) )
   allocate ( u2, source = base (203.0) )

   associate ( d => u1 )
      select type ( d )
         type is ( child )
            read ( 1, "(DT'u1'(8,3,9,4))", iostat = stat, iomsg = msg, pos = 23 ) d
            if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )       error stop 7_4
            if ( ( .not. precision_r4( d%i, 201.0 ) )  .or. &
                 ( .not. precision_r4( d%j, 202.0 ) ) )           error stop 8_4
         class default
            error stop 9_4
      end select
   end associate

   select type ( f => u2 )
      class is ( base )
         associate ( g => f )
            read ( 1, "(DT(7,2,8,3))", iostat = stat, iomsg = msg, pos = 40 ) g
            if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )       error stop 10_4
            if ( .not. precision_r4( g%i, 203.0 ) )               error stop 11_4
         end associate
      class default
         error stop 12_4
   end select

   print *, rbuffer

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child, rbuffer, idx

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(25) :: fmt

   write ( rbuffer(idx), * ) len(iotype), size(v_list), iotype, v_list
   idx = idx + 1

   select type ( dtv )
      type is ( base )
         write ( fmt, "(A2,I1,A1,I1,A1)" ) '(F', v_list(1),'.',v_list(2),')'
         read ( unit, fmt, iostat = iostat)    dtv%i
      type is ( child )
         write ( fmt, "(A2,I1,A1,I1,A2,I1,A1,I1,A1)" ) '(F', v_list(1),'.',v_list(2),',F',v_list(3),'.',v_list(4),')'
         read ( unit, fmt, iostat = iostat)    dtv%i, dtv%j
   end select
   iomsg = 'dtioread'

end subroutine