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
!*                                        array sequence derived type variable
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
      integer(4)   :: i
      character(3) :: c
   end type

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

end module

program array005
use m

   type(base), allocatable :: b1(:)
   type(base), pointer     :: b2(:,:)
   type(base)              :: b3(3) = (/ base(300,'abc'),  base(301,'def'),  base(302,'ghi') /)

   integer :: stat
   character(150) :: msg
   character(30) :: fmt = "(DT,/,DT,/,DT'_b1'(4))"

   open (1, file = 'array005.1', form='formatted', access='sequential' )

   allocate ( b1(3), source = (/ base(100,'abc'),  base(101,'def'),  base(102,'ghi') /) )
   allocate ( b2(2,2), source = reshape ( source = (/ base(200,'ABC'), base(201,'DEF'), &
                                                      base(202,'GHI'), base(203,'JKL')  /), shape = (/2,2/) ) )

10 format (DT'b2-1'(-5),/,DT'b2-2'(-6),/,DT'b2-3'(-7),/,DT'b2-4'(-8))

   write ( 1, fmt, iostat = stat, iomsg = msg )               b1((/1,2,3/))
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   write ( 1, 10, iostat = stat, iomsg = msg )                b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

   write ( 1, "(DT'b3-1'(10),/,DT'b3-2'(-10))", iostat = stat, iomsg = msg )    b3 !<- should have an extra line after the last data item written ( b3(3) )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4

end program

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base

   type(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(40) :: fmt

   write ( unit, * ) 'iotype:', iotype

   if ( size(v_list) /= 0 ) then
      write ( unit, * ) ' v_list:',v_list
      if ( v_list(1) .lt. 0 ) then
      	 write ( fmt, * ) '(" neg v_list:",I', -1*v_list(1),', 1X,A3 )'
      else
         write ( fmt, * ) '(I', v_list(1),', 1X,A3 )'
      end if
      write ( unit, fmt, iostat = iostat )    dtv%i, dtv%c
   else
      write ( unit, * ) ' empty v_list'
      write ( unit, "(I4,1X,A3)", iostat = iostat ) dtv%i, dtv%c
   end if

   iomsg = 'dtiowrite'

end subroutine
