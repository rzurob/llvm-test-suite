!*  ===================================================================
!*
!*  DATE                       : 21/03/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        Structure Component: Array Sequence Derived Type Component (read)
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
      sequence
      character(3) :: c = 'xxx'
      character(3) :: cc = 'xxx'
   end type

   type container
      sequence
      type(base), allocatable :: b1(:)
   end type

   integer :: stat
   character(150) :: msg
   character(20) :: rbuffer(8)
   integer(4) :: idx

end module

program structCompnt107
use m

   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         type(base), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   type(container) :: c1
   type(container), allocatable :: c2

   allocate( c1%b1(4)  )
   allocate( c2, source = container( (/ base('',''), base('',''), base('',''), base('','') /) ) )

   open (1, file = 'structCompnt107.1', form='formatted', access='sequential' )
   idx =1
   read ( 1, "(DT'_con1-1'(4,5),/,DT'_con1-2'(5,6),/,DT'_con1-3'(6,7),/,DT'_con1-4'(7,8))", iostat = stat, iomsg = msg )       c1%b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   read ( 1, "(4(DT'_con2'(4,5),:,/))", iostat = stat, iomsg = msg )       c2%b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4


   print *, c1%b1%c
   print *, c1%b1%cc
   print *, c2%b1%c
   print *, c2%b1%cc

   print *, rbuffer

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, rbuffer, idx

   type(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(25) :: fmt

   write ( rbuffer(idx), * ) iotype, v_list
   idx = idx + 1

   write ( fmt, "(A2,I1,A2,I1,A1)" ) '(A', v_list(1),',A',v_list(2),')'
   read ( unit, fmt, iostat = iostat )    dtv%c, dtv%cc

   iomsg = 'dtioread'

end subroutine
