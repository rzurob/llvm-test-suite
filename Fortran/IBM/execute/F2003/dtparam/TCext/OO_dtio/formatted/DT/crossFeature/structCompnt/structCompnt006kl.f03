!*  ===================================================================
!*
!*  DATE                       : 2007-06-06 (original: 21/03/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        Structure Component: Array Polymorphic Derived Type Component
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

   type base (lb)
      integer, len :: lb
      character(lb) :: c = 'xxx'
   end type

   type, extends(base) :: child (lc)
      integer, len :: lc
      character(lc) :: cc = 'xxx'
   end type

   type container1 (lc1)
      integer, len :: lc1
      class(base(lc1)), pointer :: b1(:)
   end type

   type container2 (lc2)
      integer, len :: lc2
      class(child(lc2,lc2)), allocatable :: b2(:,:)
   end type

   integer :: stat
   character(150) :: msg

end module

program structCompnt006kl
use m

   interface write(formatted)
      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(*)), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   type(container1(3))               :: c1
   class(container2(:)), allocatable :: c2

   allocate( c1%b1(4), source = (/ base(3)('abc'), base(3)('def'), base(3)('ghi'), base(3)('jkl') /)  )

   allocate( c2, source = container2(3)( reshape ( source = (/ child(3,3)('aaa','AAA'), &
             child(3,3)('bbb','BBB'), child(3,3)('ccc','CCC'), child(3,3)('ddd','DDD') /), shape = (/2,2/) ) ) )

   open (1, file = 'structCompnt006kl.1', form='formatted', access='sequential' )

   write ( 1, "(DT'_con1-1'(4),2(DT'_con1-23'(5)))", iostat = stat, iomsg = msg )       c1%b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   write ( 1, "(2(DT'_con2'(6,7),DT'_con2'(5,6)),/,4(DT'_con2base'(8)))", iostat = stat, iomsg = msg )     c2%b2, c2%b2%base
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

   deallocate ( c1%b1 )
   allocate( c1%b1(2), source = (/ child(3,3)('abc','ABC'), child(3,3)('def','DEF') /)  )

   write ( 1, "(DT'_con1-1'(4,4),DT'_con1-2'(5,5)))", iostat = stat, iomsg = msg )       c1%b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4


end program

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child

   class(base(*)), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(25) :: fmt

   write ( unit, * ) ' iotype:', iotype, ' v_list:', v_list

   select type ( dtv )
      type is ( base(*) )
         write ( fmt, "(A2,I1,A1)" ) '(A', v_list(1),')'
         write ( unit, fmt, iostat = iostat )    dtv%c
      type is ( child(*,*) )
         write ( fmt, "(A2,I1,A2,I1,A1)" ) '(A', v_list(1),',A',v_list(2),')'
         write ( unit, fmt, iostat = iostat )    dtv%c, dtv%cc
   end select
   iomsg = 'dtiowrite'

end subroutine
