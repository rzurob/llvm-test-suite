!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : fdtedit107kl
!*
!*  PROGRAMMER                 : David Forster (derived from fdtedit107 by Robert Ma)
!*  DATE                       : 2007-06-08 (original: 21/03/2005)
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
!*                                        multiple DT edit descriptor in an I/O statement (write)
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
      integer(kb) :: i
   end type

   type, extends(base) :: child (kc)
      integer, kind :: kc
      integer(kc) :: j
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

program fdtedit107kl
use m

   class(base(4)), allocatable :: b1
   type(base(4))               :: b2 = base(4)(200)
   class(child(4,4)), pointer    :: c1
   type(child(4,4))              :: c2 = child(4,4)(30,40)

   integer :: stat
   character(150) :: msg
   character(33) :: fmt = "(DT,DT'b2',DT'c1'(1),DT'c2'(1,2))"
   open (1, file = 'fdtedit107kl.1', form='formatted', access='sequential' )

   allocate ( b1, source = base(4)(100) )
   allocate ( c1, source = child(4,4)(10,20) )
   write ( 1, fmt, iostat = stat, iomsg = msg )      b1, b2, c1, c2
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
         write ( unit, *, iostat = iostat ) iotype, v_list, dtv%i
      type is ( child(4,4) )
         write ( unit, *, iostat = iostat ) iotype, v_list, dtv%i, dtv%j
   end select
   iomsg = 'dtiowrite'

end subroutine
