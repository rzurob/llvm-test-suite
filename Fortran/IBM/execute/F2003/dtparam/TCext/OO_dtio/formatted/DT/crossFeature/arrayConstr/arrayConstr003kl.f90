!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : arrayConstr003kl
!*
!*  PROGRAMMER                 : David Forster (derived from arrayConstr003 by Robert Ma)
!*  DATE                       : 2007-06-06 (original: 21/03/2005)
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
!*                                        Array Constructor for sequence type
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

   type base (kb,lb)
      integer, kind :: kb
      integer, len :: lb
      sequence
      integer(kb) :: i
      character(lb) :: c
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

   integer :: stat
   character(150) :: msg

end module

program arrayConstr003kl
use m

   type(base(4,3)) :: b1 = base(4,3)(102,'def')
   type(base(4,3)) :: b2(3) = (/ base(4,3)(201,'abc'), base(4,3)(202,'def'), base(4,3)(203,'ghi') /)

   open (1, file = 'arrayConstr003kl.1', form='formatted', access='sequential' )

   write ( 1, "(DT'_base-1'(4,4),/,DT'_base-2'(5,5),/,DT'_base-3'(6,6))", iostat = stat, iomsg = msg )   (/ base(4,3)(101,'abc'), b1, base(4,3)(103,'ghi') /)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   write ( 1, "(DT'_base-1'(5,5))", iostat = stat, iomsg = msg )   (/ ( b2(i), base(4,3)(i+300,'ibm'), i = 1, 3 ) /)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4


end program

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base

   type(base(4,*)), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(25) :: fmt

   write ( unit, * ) ' iotype:', iotype, ' v_list:', v_list

   write ( fmt, "(A2,I1,A2,I1,A1)" ) '(I', v_list(1),',A',v_list(2),')'
   write ( unit, fmt, iostat = iostat )    dtv%i, dtv%c

   iomsg = 'dtiowrite'

end subroutine
