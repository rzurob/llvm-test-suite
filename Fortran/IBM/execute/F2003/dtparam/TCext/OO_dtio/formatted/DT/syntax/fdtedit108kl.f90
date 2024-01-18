!*  ===================================================================
!*
!*  TEST CASE NAME             : fdtedit108kl
!*
!*  DATE                       : 2007-06-08 (original: 21/03/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        multiple DT edit descriptor in an I/O statement (write)
!*                                        each item of different type
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
   type base (kb)
      integer, kind :: kb
      integer(kb)   :: i
   end type

   type, extends(base) :: child (kc)
      integer, kind :: kc
      integer(kc)   :: j
   end type

   type base1 (lb1)
      integer, len :: lb1
      character(lb1) :: c
   end type

   type base2 (lb2)
      integer, len :: lb2
      sequence
      character(lb2) :: c
   end type

   interface write(formatted)
      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(4)), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
      subroutine writeformattedbase1(dtv, unit, iotype, v_list, iostat, iomsg )
         import base1
         class(base1(*)), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
      subroutine writeformattedbase2(dtv, unit, iotype, v_list, iostat, iomsg )
         import base2
         type(base2(*)), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module

program fdtedit108kl
use m

   class(base(4)), allocatable :: b1
   class(base(4)), pointer     :: b2
   class(base1(:)), pointer    :: b3
   type(base2(3))              :: b4 = base2(3)('DEF')

   integer :: stat
   character(150) :: msg
   character(33) :: fmt = "(DT,DT'b2',DT'b3'(1),DT'b4'(1,2))"

   allocate ( b1, source = base(4)( 10 ) )
   allocate ( b2, source = child(4,4)( 20, 30 ) )
   allocate ( b3, source = base1(3)( 'ABC' ) )

   open (1, file = 'fdtedit108kl.1', form='formatted', access='sequential' )

   write ( 1, fmt, iostat = stat, iomsg = msg )      b1, b2, b3, b4
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) )  error stop 1_4

end program

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child

   class(base(4)), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   select type ( dtv )
      type is ( base(4) )
         write ( unit, *, iostat = iostat ) ' BASE:', iotype, v_list, ' dtv%i=', dtv%i
      type is ( child(4,4) )
         write ( unit, *, iostat = iostat ) ' CHILD:',iotype, v_list, ' dtv%i=', dtv%i, 'dtv%j=', dtv%j
   end select
   iomsg = 'dtiowrite'

end subroutine

subroutine writeformattedbase1 (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base1

   class(base1(*)), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write ( unit, *, iostat = iostat ) ' BASE1:', iotype, v_list,'dtv%c=', dtv%c

   iomsg = 'dtiowrite'

end subroutine

subroutine writeformattedbase2 (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base2

   type(base2(*)), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write ( unit, *, iostat = iostat ) ' BASE2:', iotype, v_list,'dtv%c=', dtv%c

   iomsg = 'dtiowrite'

end subroutine

