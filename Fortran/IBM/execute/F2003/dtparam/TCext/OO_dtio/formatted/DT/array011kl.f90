!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : array011kl
!*
!*  PROGRAMMER                 : David Forster (derived from array011 by Robert Ma)
!*  DATE                       : 2007-06-04 (original: 21/03/2005)
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
!*                                        zero-sized array and array of zero storage derived type
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

   type zerobase
   end type

   type base (k)
      integer, kind :: k
      integer(k) :: i
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
      subroutine writeformattedzerobase(dtv, unit, iotype, v_list, iostat, iomsg )
         import zerobase
         class(zerobase), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module

program array011kl
use m

   class(base(4)), allocatable :: b1(:)
   type(base(4))               :: b2(0)

   class(zerobase), allocatable :: z1(:)
   type(zerobase)               :: z2(4)

   integer :: stat
   character(150) :: msg
   character(31) :: fmt = "(DT//)"

   open (1, file = 'array011kl.1', form='formatted', access='sequential' )

   allocate ( b1(1:0) )
   allocate ( z1(5) )

10 format (DT//)
   msg = ''

   write ( 1, fmt, iostat = stat, iomsg = msg )               b1
   if ( ( stat /= 0 ) .or. ( msg /= '' ) ) error stop 1_4

   write ( 1, 10, iostat = stat, iomsg = msg )                b2
   if ( ( stat /= 0 ) .or. ( msg /= '' ) ) error stop 2_4

   write ( 1, "(DT'z1-1'(1,2),/,DT'z1-2'(-3,-4),/,DT'z1-3'(5,6),/,DT'z1-4'(-7,-8),/,DT'z1-5'(9,10))", iostat = stat, iomsg = msg )   z1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4

   write ( 1, "(DT'z2-1'(11,12),DT'z2-2'(-13,-14))", iostat = stat, iomsg = msg )   z2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 4_4

end program

subroutine writeformattedzerobase (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: zerobase

   class(zerobase), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write ( unit, * )   'iotype: ', iotype, ' v_list:',  v_list

   iomsg = 'dtiowrite'

end subroutine

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base

   class(base(4)), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write ( unit, * ) iotype, v_list, dtv%i

   iomsg = 'dtiowrite'

end subroutine
