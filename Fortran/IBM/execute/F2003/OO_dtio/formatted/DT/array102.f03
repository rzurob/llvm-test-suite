!*  ===================================================================
!*
!*  DATE                       : 21/03/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        array polymorphic derived type variable (read)
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
      integer(4) :: i = -999
   end type

   type, extends(base) :: child
      character(3) :: c = 'xxx'
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

   integer :: idx
   character(10) :: rbuffer(16)

end module

program array102
use m

   class(base), allocatable  :: b1(:)
   class(base), pointer      :: b2(:,:)
   class(child), allocatable :: c1
   class(child), pointer     :: c2

   dimension :: c1(:)
   dimension :: c2(:,:)

   integer :: stat
   character(150) :: msg
   character(30) :: fmt = "(DT'b1-1',DT'b1-2'(4))"

   open (1, file = 'array102.1', form='formatted', access='sequential' )

   allocate ( b1(4) )
   allocate ( child :: b2(2,2) )
   allocate ( c1(4) )
   allocate ( c2(2,2) )

10 format (DT'b2')

   idx =1

   read ( 1, fmt, iostat = stat, iomsg = msg )               b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   read ( 1, 10, iostat = stat, iomsg = msg )                b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4

   read ( 1, "(DT'c1-1'(10),/,DT'c1-2'(10),/,DT'c1-3'(10),/,DT'c1-4'(10))", iostat = stat, iomsg = msg )    c1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 3_4

   read ( 1, "(DT'c2'(10,5))", iostat = stat, iomsg = msg )  c2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 4_4

   if ( ( b1(1)%i /= 100 ) .or. ( b1(2)%i /= 101 ) .or. ( b1(3)%i /= 102 ) .or. ( b1(4)%i /= 103 ) )                       error stop 4_4

   select type ( b2 )
      type is ( child )
         if ( ( b2(1,1)%i /= 200 ) .or. ( b2(2,1)%i /= 201 ) .or. ( b2(1,2)%i /= 202 ) .or. ( b2(2,2)%i /= 203 )   .or. &
              ( b2(1,1)%c /= 'abc' ) .or. ( b2(2,1)%c /= 'def' ) .or. ( b2(1,2)%c /= 'ghi' ) .or. ( b2(2,2)%c /= 'jkl' ) ) error stop 5_4
   end select

   if ( ( c1(1)%i /= 300 ) .or. ( c1(2)%i /= 301 ) .or. ( c1(3)%i /= 302 ) .or. ( c1(4)%i /= 303 )   .or. &
        ( c1(1)%c /= 'ABC' ) .or. ( c1(2)%c /= 'DEF' ) .or. ( c1(3)%c /= 'GHI' ) .or. ( c1(4)%c /= 'JKL' ) )               error stop 6_4

   if ( ( c2(1,1)%i /= 400 ) .or. ( c2(2,1)%i /= 401 ) .or. ( c2(1,2)%i /= 402 ) .or. ( c2(2,2)%i /= 403 )   .or. &
        ( c2(1,1)%c /= 'ABC' ) .or. ( c2(2,1)%c /= 'DEF' ) .or. ( c2(1,2)%c /= 'GHI' ) .or. ( c2(2,2)%c /= 'JKL' ) )       error stop 7_4

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

   character(20) :: fmt

   write ( rbuffer(idx), * ) iotype, v_list
   idx = idx + 1

   select type ( dtv )
      type is (base)
         read ( unit, "(I4)", iostat = iostat ) dtv%i
      type is (child)
         if ( size(v_list) == 0 ) then
            read ( unit, *, iostat = iostat ) dtv%i, dtv%c
         else if ( size(v_list) == 1 ) then
            write ( fmt, * ) '(I', v_list(1),')'
            read ( unit, fmt, iostat = iostat )    dtv%i
            read ( unit, *, iostat = iostat )      dtv%c
         else if ( size(v_list) == 2 ) then
            write ( fmt, * ) '(I', v_list(1),',A',v_list(2),')'
            read ( unit, fmt, iostat = iostat )    dtv%i, dtv%c
         end if
   end select

   iomsg = 'dtioread'

end subroutine
