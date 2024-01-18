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
   type base
      integer(4)   :: i
   end type

   type, extends(base) :: child
      integer(4)   :: j
   end type

   type base1
      character(3) :: c
   end type

   type base2
      sequence
      character(3) :: c
   end type

   interface write(formatted)
      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
      subroutine writeformattedbase1(dtv, unit, iotype, v_list, iostat, iomsg )
         import base1
         class(base1), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
      subroutine writeformattedbase2(dtv, unit, iotype, v_list, iostat, iomsg )
         import base2
         type(base2), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module

program fdtedit108
use m

   class(base), allocatable :: b1
   class(base), pointer     :: b2
   class(base1), pointer    :: b3
   type(base2)              :: b4 = base2('DEF')

   integer :: stat
   character(150) :: msg
   character(33) :: fmt = "(DT,DT'b2',DT'b3'(1),DT'b4'(1,2))"

   allocate ( b1, source = base( 10 ) )
   allocate ( b2, source = child( 20, 30 ) )
   allocate ( b3, source = base1( 'ABC' ) )

   open (1, file = 'fdtedit108.1', form='formatted', access='sequential' )

   write ( 1, fmt, iostat = stat, iomsg = msg )      b1, b2, b3, b4
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) )  error stop 1_4

end program

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child

   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   select type ( dtv )
      type is ( base )
         write ( unit, *, iostat = iostat ) ' BASE:', iotype, v_list, ' dtv%i=', dtv%i
      type is ( child )
         write ( unit, *, iostat = iostat ) ' CHILD:',iotype, v_list, ' dtv%i=', dtv%i, 'dtv%j=', dtv%j
   end select
   iomsg = 'dtiowrite'

end subroutine

subroutine writeformattedbase1 (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base1

   class(base1), intent(in) :: dtv
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

   type(base2), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write ( unit, *, iostat = iostat ) ' BASE2:', iotype, v_list,'dtv%c=', dtv%c

   iomsg = 'dtiowrite'

end subroutine

