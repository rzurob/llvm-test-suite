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
!*                                        array non-polymorphic derived type variable (read)
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
   character(10) :: rbuffer(11)

end module

program array101
use m

   type(base), allocatable :: b1(:)
   type(base), pointer     :: b2(:,:)
   type(base)              :: b3(4) = (/ base(), base(), base(), base() /)

   integer :: stat
   character(150) :: msg
   character(30) :: fmt = "(DT)"

   open (1, file = 'array101.1', form='formatted', access='sequential' )

   allocate ( b1(3) )
   allocate ( b2(2,2) )

10 format (DT'b2-1'(5),DT'b2-2'(10))
   idx = 1
   read ( 1, fmt, iostat = stat, iomsg = msg )               b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   read ( 1, 10, iostat = stat, iomsg = msg )                b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4

   read ( 1, "(DT'b3-1'(5), DT'b3-2'(10), DT'b3-3'(15))", iostat = stat, iomsg = msg )    b3
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 3_4
   
   if ( ( b1(1)%i /= 100 ) .or. ( b1(2)%i /= 101 ) .or. ( b1(3)%i /= 102 ) )                                 error stop 4_4
   if ( ( b2(1,1)%i /= 200 ) .or. ( b2(2,1)%i /= 201 ) .or. ( b2(1,2)%i /= 202 ) .or. ( b2(2,2)%i /= 203 ) ) error stop 5_4
   if ( ( b3(1)%i /= 300 ) .or. ( b3(2)%i /= 301 ) .or. ( b3(3)%i /= 302 ) .or. ( b3(4)%i /= 303 ) )         error stop 6_4
   
   print *, rbuffer

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, rbuffer, idx

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(20) :: fmt
   write ( rbuffer(idx), * ) iotype, v_list
   idx = idx + 1

   if ( size(v_list) /= 0 ) then
      write ( fmt, * ) '(I', v_list(1),')'
      read ( unit, fmt, iostat = iostat )    dtv%i
   else
      read ( unit, "(I4)", iostat = iostat ) dtv%i
   end if


   iomsg = 'dtioread'

end subroutine
