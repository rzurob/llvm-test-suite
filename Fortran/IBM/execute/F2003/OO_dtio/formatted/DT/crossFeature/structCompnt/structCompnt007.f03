!*  ===================================================================
!*
!*  DATE                       : 21/03/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        Structure Component: Array Sequence Derived Type Component
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

end module

program structCompnt007
use m

   interface write(formatted)
      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         type(base), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   type(container) :: c1
   type(container), allocatable :: c2

   allocate( c1%b1(4), source = (/ base('abc','ABC'), base('def','DEF'), base('ghi','GHI'), base('jkl','JKL') /)  )
   allocate( c2, source = container( (/ base('mno','MNO'), base('pqr','PQR'), base('stu','STU'), base('vwx','VWX') /) ) )

   open (1, file = 'structCompnt007.1', form='formatted', access='sequential' )

   write ( 1, "(DT'_con1-1'(4,5),/,DT'_con1-2'(5,6),/,DT'_con1-3'(6,7),/,DT'_con1-4'(7,8))", iostat = stat, iomsg = msg )       c1%b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   write ( 1, "(4(DT'_con2'(4,5),:,/))", iostat = stat, iomsg = msg )       c2%b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

end program

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base

   type(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(25) :: fmt

   write ( unit, * ) ' iotype:', iotype, ' v_list:', v_list

   write ( fmt, "(A2,I1,A2,I1,A1)" ) '(A', v_list(1),',A',v_list(2),')'
   write ( unit, fmt, iostat = iostat )    dtv%c, dtv%cc

   iomsg = 'dtiowrite'

end subroutine
