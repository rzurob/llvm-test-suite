!*  ===================================================================
!*
!*  TEST CASE NAME             : recursive004kl
!*
!*  DATE                       : 2007-06-05 (original: 21/03/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        derived type containing linked list component
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
      class(base(kb)), pointer :: next => null()
      integer(kb) :: i
   end type

   type, extends(base) :: child (kc)
      integer, kind :: kc
      integer(kc) :: j
   end type

   type linkedlist (kll)
      integer, kind :: kll
      class(base(kll)), pointer :: head => null()
   end type

   interface write(formatted)

      subroutine writeformattedll(dtv, unit, iotype, v_list, iostat, iomsg )
         import linkedlist
         class(linkedlist(4)), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine

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

end module

program recursive004kl
use m

   class(linkedlist(4)), allocatable :: ll

   integer :: stat
   character(150) :: msg
   character(30) :: fmt = "(DT(5,6),/)"

   open (1, file = 'recursive004kl.1', form='formatted', access='sequential' )

   allocate ( ll )

   allocate ( ll%head, source = child(4,4)(null(),100,101) )
   allocate ( ll%head%next, source = child(4,4)(null(), 200, 201) )
   allocate ( ll%head%next%next, source = base(4)(null(), 300) )
   allocate ( ll%head%next%next%next, source = child(4,4)(null(), 400, 401) )
   allocate ( ll%head%next%next%next%next, source = child(4,4)(null(), 500, 501) )

   write ( 1, fmt, iostat = stat, iomsg = msg )                ll
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   fmt = "(DT'_ll'(4,5))"

   write ( 1, fmt, iostat = stat, iomsg = msg )                ll
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

end program

recursive subroutine writeformattedll (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child, linkedlist, write(formatted)

   class(linkedlist(4)), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(22) :: fmt

   write ( unit, * ) 'Linked list: ', ' iotype:', iotype, ' v_list:', v_list

   write ( fmt, * ) '(/,DT"base"(', v_list(1),',', v_list(2),'))'
   write ( unit, fmt, iostat = iostat, iomsg = iomsg )    dtv%head

   if ( ( iostat /= 0 ) .or. ( iomsg /= 'basewrite' ) )   error stop 3_4

   iomsg = 'dtiowrite'

end subroutine

recursive subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child, write(formatted)

   class(base(4)), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(25) :: fmt

   write ( unit, *, iostat = iostat )      'iotype:', iotype
   write ( fmt, * ) '(I', v_list(1),')'
   write ( unit, fmt, iostat = iostat )    dtv%i

   select type ( dtv )
      type is ( child(4,4) )
         write ( fmt, * ) '(I', v_list(2),')'
         write ( unit, fmt, iostat = iostat )    dtv%j
   end select

   if ( associated ( dtv%next ) ) then
      write ( fmt, * ) '(/,DT"base"(',v_list(1)+1,',',v_list(2)+1,'))'
      write ( unit, fmt, iostat = iostat, iomsg = iomsg ) dtv%next
      if ( ( iostat /= 0 ) .or. ( iomsg /= 'basewrite' ) )  error stop 4_4
   end if

   iomsg = 'basewrite'

end subroutine
