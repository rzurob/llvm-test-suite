!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : recursive002kl
!*
!*  PROGRAMMER                 : David Forster (derived from recursive002 by Robert Ma)
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
!*                                        recursively write a polymorphic linked list
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
      integer(kb) :: i = -999
   end type

   type, extends(base) :: child (kc)
      integer, kind :: kc
      integer(kc) :: j = -999
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

program recursive002kl
use m

   class(base(4)), allocatable :: head

   integer :: stat
   character(150) :: msg
   character(30) :: fmt = "(DT(5,6),/)"

   open (1, file = 'recursive002kl.1', form='formatted', access='sequential' )

   allocate ( head, source = child(4,4)(null(), 100,101) )
   allocate ( head%next, source = child(4,4)(null(), 200, 201) )
   allocate ( head%next%next, source = base(4)(null(), 300) )
   allocate ( head%next%next%next, source = child(4,4)(null(), 400,401) )
   allocate ( head%next%next%next%next, source = child(4,4)(null(), 500,501) )

   idx = 1

   write ( 1, fmt, iostat = stat, iomsg = msg )              head
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   fmt = "(DT'_ll'(4,5))"

   write ( 1, fmt, iostat = stat, iomsg = msg )              head
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

end program

recursive subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child, write(formatted), idx

   class(base(4)), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(20) :: fmt

   write ( unit, * ) 'element number', idx, ' iotype:', iotype, ' v_list:', v_list
   
   write ( fmt, * ) '(I', v_list(1),')'
   write ( unit, fmt, iostat = iostat )    dtv%i
   
   select type ( dtv )
      type is ( child(4,4) )
         write ( fmt, * ) '(I', v_list(2),')'
         write ( unit, fmt, iostat = iostat )    dtv%j
   end select

   if ( associated ( dtv%next ) ) then
      idx = idx + 1
      write ( fmt, * ) '(/,DT(',v_list(1)+1,',',v_list(2)+1,'))'
      write ( unit, fmt, iostat = iostat, iomsg = iomsg ) dtv%next
      if ( ( iostat /= 0 ) .or. ( iomsg /= 'dtiowrite' ) )  error stop 3_4
   end if

   iomsg = 'dtiowrite'

end subroutine
