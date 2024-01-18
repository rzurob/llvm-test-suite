!*  ===================================================================
!*
!*  TEST CASE NAME             : scalar007kl
!*
!*  DATE                       : 2007-06-05 (original: 21/03/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        scalar sequence derived type variable containing sequence components which uses DTIO
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

   type data (kd)
      integer, kind :: kd
      sequence
      integer(kd) :: i
   end type

   type base (kb,lb)
      integer, kind :: kb
      integer, len :: lb
      sequence
      type(data(4))   :: d
      character(3) :: c
   end type

   interface write(formatted)
      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         type(base(4,*)), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module

program scalar007kl
use m

   type(base(4,:)), allocatable :: b1
   type(base(4,:)), pointer     :: b2
   type(base(4,3))              :: b3 = base(4,3)(data(4)(300),'ghi')

   integer :: stat
   character(150) :: msg
   character(30) :: fmt = "(DT(5))"

   open (1, file = 'scalar007kl.1', form='formatted', access='sequential' )

   allocate ( b1, source = base(4,3)(data(4)(100),'abc') )
   allocate ( b2, source = base(4,3)(data(4)(200),'def') )

10 format (DT'b2'(6,7))

   write ( 1, fmt, iostat = stat, iomsg = msg )               b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   write ( 1, 10, iostat = stat, iomsg = msg )                b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

   write ( 1, "(DT'b3'(8,9))", iostat = stat, iomsg = msg )    b3
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4

end program

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, data

   interface write(formatted)
      subroutine writeformatteddata(dtv, unit, iotype, v_list, iostat, iomsg )
         import data
         type(data(4)), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   type(base(4,*)), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(25) :: fmt

   write ( unit, * ) 'iotype: ', iotype
   iomsg = ''

   if ( size(v_list) == 1 ) then
      write ( unit, * ) ' Data not using DTIO: v_list is:', v_list
      write ( fmt, * ) '(I', v_list(1),', 1X,A3 )'
      write ( unit, fmt, iostat = iostat, iomsg = iomsg )    dtv%d%i, dtv%c
   else if ( size(v_list) == 2 ) then
      write ( unit, * ) 'Data using DTIO:    v_list is:', v_list
      write ( fmt, * ) '(DT(', v_list(1),'),1X,A', v_list(2) ,')'
      write ( unit, fmt, iostat = iostat, iomsg = iomsg )          dtv%d, dtv%c
      if ( ( iostat /= 0 )  .or. ( iomsg /= 'datawrite' )  ) error stop 4_4
   end if

   iomsg = 'dtiowrite'

end subroutine

subroutine writeformatteddata (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: data

   type(data(4)), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(4) :: fmt
   write ( fmt, "(A2,I1,A1)" )  '(I', v_list(1),')'
   write ( unit, fmt, iostat = iostat )      dtv%i

   iomsg = 'datawrite'

end subroutine


