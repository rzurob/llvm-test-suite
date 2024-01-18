!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : scalar003kl
!*
!*  PROGRAMMER                 : David Forster (derived from scalar003 by Robert Ma)
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
!*                                        scalar (non-)polymorphic derived type variable with non-polymorphic component
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
      integer(kd) :: i
   end type

   type base (kb)
      integer, kind :: kb
      type(data(kb)) :: d
      integer(kb) :: j
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
   end interface

end module

program scalar003kl
use m

   class(base(4)), allocatable :: b1
   type(base(4)), pointer      :: b2
   type(base(4))               :: b3 = base(4)(data(4)(103), 1003)

   integer :: stat
   character(150) :: msg
   character(30) :: fmt = "(DT(5,6))"

   open (1, file = 'scalar003kl.1', form='formatted', access='sequential' )

   allocate ( b1, source = base(4)(data(4)(101), 1001) )
   allocate ( b2, source = base(4)(data(4)(102), 1002) )

10 format (DT'b2'(7,8) )

   write ( 1, fmt, iostat = stat, iomsg = msg )               b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   write ( 1, 10, iostat = stat, iomsg = msg )                b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

   write ( 1, "(DT'b3'(9,10))", iostat = stat, iomsg = msg )   b3
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4

end program

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, data

   class(base(4)), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(30) :: fmt

   if ( size(v_list) /= 2 ) error stop 4_4
   
   write ( fmt, * ) "( A, I4, I4, I", v_list(1), ", I", v_list(2), ")"
   write ( unit, fmt ) iotype, v_list, dtv%d, dtv%j

   iomsg = 'dtiowrite'

end subroutine
