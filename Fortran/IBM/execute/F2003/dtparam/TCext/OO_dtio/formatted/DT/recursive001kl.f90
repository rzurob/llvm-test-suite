!*  ===================================================================
!*
!*  TEST CASE NAME             : recursive001kl
!*
!*  DATE                       : 2007-06-05 (original: 21/03/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        recursively write a non-polymorphic linked list
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
      type(base(kb)), pointer :: next => null()
      integer(kb) :: i = -999
   end type

   interface write(formatted)
      recursive subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(4)), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   integer(4) :: idx

end module

program recursive001kl
use m

   type(base(4)), allocatable :: head

   integer :: stat
   character(150) :: msg
   character(30) :: fmt = "(DT(5))"

   open (1, file = 'recursive001kl.1', form='formatted', access='sequential' )

   allocate ( head, source = base(4)(null(), 100) )
   allocate ( head%next, source = base(4)(null(), 200) )
   allocate ( head%next%next, source = base(4)(null(), 300) )
   allocate ( head%next%next%next, source = base(4)(null(), 400) )
   allocate ( head%next%next%next%next, source = base(4)(null(), 500) )

   idx = 1

   write ( 1, fmt, iostat = stat, iomsg = msg )              head
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   fmt = "(DT'_ll'(15))"

   write ( 1, fmt, iostat = stat, iomsg = msg )              head
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

end program

recursive subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, write(formatted), idx

   class(base(4)), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(20) :: fmt

   write ( unit, * ) 'element number', idx, ' iotype:', iotype, ' v_list:', v_list

   write ( fmt, * ) '(I', v_list(1),',/)'
   write ( unit, fmt, iostat = iostat )    dtv%i

   if ( associated ( dtv%next ) ) then
      idx = idx + 1
      write ( fmt, * ) '(DT(',v_list(1),'))'
      write ( unit, fmt, iostat = iostat, iomsg = iomsg ) dtv%next
      if ( ( iostat /= 0 ) .or. ( iomsg /= 'dtiowrite' ) )  error stop 3_4
   end if

   iomsg = 'dtiowrite'

end subroutine
