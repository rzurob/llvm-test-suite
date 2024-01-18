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
!*                                        Structure Component: Scalar Polymorphic Derived Type Component (read)
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
      character(3) :: c = 'xxx'
   end type

   type, extends(base) :: child
      character(3) :: cc = 'xxx'
   end type

   type container1
      class(base), allocatable :: b1
   end type

   type container2
      class(child), pointer    :: b2
   end type

   integer :: stat
   character(150) :: msg
   character(20) :: rbuffer(5)
   integer(4) :: idx

end module

program structCompnt102
use m

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

   type(container1)               :: c1
   class(container2), allocatable :: c2

   c1 = container1(base(''))
   allocate( c2, source = container2(null()))
   allocate( c2%b2, source = child('','') )

   open (1, file = 'structCompnt102.1', form='formatted', access='sequential' )
   idx =1
   read ( 1, "(DT'_con1'(4))", iostat = stat, iomsg = msg )       c1%b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   read ( 1, "(DT'_con2'(6,7),DT'_con2base'(8))", iostat = stat, iomsg = msg )     c2%b2, c2%b2%base
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4

   if ( c1%b1%c /= 'ABC' )                               error stop 3_4
   if ( ( c2%b2%c /= 'JKL' ) .or. ( c2%b2%cc /= 'GHI' ) )error stop 4_4

   c1 = container1(child('',''))

   select type ( g => c1%b1 )
      type is ( child )
         read ( 1, "(DT'_con1'(7,8),DT'_con1base'(9))", iostat = stat, iomsg = msg )     c1%b1, g%base
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )        error stop 5_4
         if ( ( g%c /= 'PQR' ) .or. ( g%cc /= 'MNO' ) ) error stop 6_4
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

   write ( rbuffer(idx), * ) iotype, v_list
   idx = idx + 1

   select type ( dtv )
      type is ( base )
         write ( fmt, "(A2,I1,A1)" ) '(A', v_list(1),')'
         read ( unit, fmt, iostat = iostat )    dtv%c
      type is ( child )
         write ( fmt, "(A2,I1,A2,I1,A1)" ) '(A', v_list(1),',A',v_list(2),')'
         read ( unit, fmt, iostat = iostat )    dtv%c, dtv%cc
   end select
   iomsg = 'dtioread'

end subroutine
