!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : recursive003kl
!*
!*  PROGRAMMER                 : David Forster (derived from recursive003 by Robert Ma)
!*  DATE                       : 2007-06-05 (original: 21/03/2005)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        recursively write a polymorphic linked list (contains inner type)
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
      integer(kd) :: i = -999
   end type

   type base (kb)
      integer, kind :: kb
      class(base(kb)), pointer :: next => null()
      type(data(kb)) :: d1
   end type

   type, extends(base) :: child (kc)
      integer, kind :: kc
      type(data(kc)) :: d2
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

program recursive003kl
use m

   class(base(4)), allocatable :: head

   integer :: stat
   character(150) :: msg
   character(30) :: fmt = "(DT(-5,-6),/)"

   open (1, file = 'recursive003kl.1', form='formatted', access='sequential' )

   allocate ( head, source = child(4,4)(null(), data(4)(100),data(4)(101)) )
   allocate ( head%next, source = child(4,4)(null(), data(4)(200), data(4)(201)) )
   allocate ( head%next%next, source = base(4)(null(), data(4)(300)) )
   allocate ( head%next%next%next, source = child(4,4)(null(), data(4)(400),data(4)(401)) )
   allocate ( head%next%next%next%next, source = child(4,4)(null(), data(4)(500),data(4)(501)) )

   idx = 1

   write ( 1, fmt, iostat = stat, iomsg = msg )              head
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   fmt = "(DT'_ll'(-4,-5))"

   write ( 1, fmt, iostat = stat, iomsg = msg )              head
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

end program

recursive subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child, write(formatted), idx, data

   class(base(4)), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(20) :: fmt

   interface write(formatted)
      subroutine writeformatteddata(dtv, unit, iotype, v_list, iostat, iomsg )
         import data
         class(data(4)), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   write ( unit, * ) 'element number', idx, ' iotype:', iotype, ' v_list:', v_list

   write ( fmt, * ) '(DT(', -1*v_list(1),'))'
   write ( unit, fmt, iostat = iostat )    dtv%d1

   select type ( dtv )
      type is ( child(4,4) )
         write ( fmt, * ) '(DT(', -1*v_list(2),'))'
         write ( unit, fmt, iostat = iostat )    dtv%d2
   end select

   if ( associated ( dtv%next ) ) then
      idx = idx + 1
      write ( fmt, * ) '(/,DT(',v_list(1)-1,',',v_list(2)-1,'))'
      write ( unit, fmt, iostat = iostat, iomsg = iomsg ) dtv%next
      if ( ( iostat /= 0 ) .or. ( iomsg /= 'dtiowrite' ) )  error stop 3_4
   end if

   iomsg = 'dtiowrite'

end subroutine

subroutine writeformatteddata (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: data

   class(data(4)), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(20) :: fmt

   write ( fmt, * ) '(I', v_list(1),')'
   write ( unit, fmt, iostat = iostat )    dtv%i

   iomsg = 'dtiowrite'

end subroutine
