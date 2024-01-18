!*  ===================================================================
!*
!*  TEST CASE NAME             : structConstr002kl
!*
!*  DATE                       : 2007-06-07 (original: 21/03/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        Structure Constructor: Scalar Sequence Type Entities
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
      sequence
      character(lb) :: c = 'xxx'
      character(lb) :: cc = 'xxx'
   end type

   integer :: stat
   character(150) :: msg

end module

program structConstr002kl
use m

   interface write(formatted)
      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         type(base(*)), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   open (1, file = 'structConstr002kl.1', form='formatted', access='sequential' )

   write ( 1, "(DT'_base-1'(4,4),/,DT'_base-2'(5,5),/,DT'_base-3'(6,6):,'junk')", iostat = stat, iomsg = msg )   base(3)('abc','ABC'), base(3)('def','DEF'), base(3)('ghi','GHI')
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   write ( 1, "(DT'_io1'(5,5),DT'_io2'(6,7))", iostat = stat, iomsg = msg )                    ( base(3)('ABC','abc'),base(3)('DEF','def'),i= 0,2 )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

   msg = ''

   write ( 1, "(A4,A4,A4,A4)", iostat = stat, iomsg = msg )                    base(3)('ghi','GHI'),base(3)('jkl','JKL') !<- shall not call DTIO procedure
   if ( ( stat /= 0 ) .or. ( msg /= '' ) ) error stop 3_4

end program

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base

   type(base(*)), intent(in) :: dtv
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
