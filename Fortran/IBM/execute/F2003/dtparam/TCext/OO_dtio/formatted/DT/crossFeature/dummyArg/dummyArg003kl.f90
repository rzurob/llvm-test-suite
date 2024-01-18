!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArg003kl
!*
!*  DATE                       : 2007-06-06 (original: 21/03/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        Argument Association: explicit shape array dummy argument
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
      integer(kb) :: i = -999
   end type

   type, extends(base) :: child (kc)
      integer, kind :: kc
      integer(kc) :: j = -999
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

   integer :: stat
   character(150) :: msg

   contains

      subroutine foo ( dtv )
         type(base(4)), intent(in) :: dtv(3)
         character(63) :: fmt = "(DT'_type_base1'(5),/,DT'_type_base2'(6),/, DT'_type_base3'(7))"

         write (1, fmt, iostat = stat, iomsg = msg ) dtv
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) )  error stop 1_4

      end subroutine

      subroutine bar ( dtv, lb, ub )
         class(base(4)), intent(inout) :: dtv(lb:ub)
         integer, intent(in) :: lb,ub
      10 format (DT'_class_base1'(4,5),/,DT'_class_base1'(5,6),/,DT'_class_base1'(6,7))

         write (1, 10, iostat = stat, iomsg = msg ) dtv

         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) )  error stop 2_4

      end subroutine

end module

program dummyArg003kl
use m

   type(base(4)), allocatable :: b1(:)
   type(base(4))              :: b2(3) = (/ base(4)(111), base(4)(112), base(4)(113) /)
   class(base(4)), pointer    :: b3(:)

   type(child(4,4)), allocatable :: c1(:)
   type(child(4,4))              :: c2(3) = (/ child(4,4)(211,212), child(4,4)(213,214), child(4,4)(215,216) /)
   class(child(4,4)), pointer    :: c3(:)

   open (1, file = 'dummyArg003kl.1', form='formatted', access='sequential' )

   allocate ( b1(3:5), source = (/ base(4)(101), base(4)(102), base(4)(103) /) )
   allocate ( b3(3),   source = (/ child(4,4)(121,122), child(4,4)(123,124), child(4,4)(125,126) /) )

   allocate ( c1(3), source = (/ child(4,4)(201,202), child(4,4)(203,204), child(4,4)(205,206) /) )
   allocate ( c3(4:6), source = (/ child(4,4)(221,222), child(4,4)(223,224), child(4,4)(225,226) /) )

   call foo ( b1 )
   call foo ( b2 )
   call foo ( b3 )
   call bar ( b1, lbound(b1,1), ubound(b1,1) )
   call bar ( b2, lbound(b2,1), ubound(b2,1) )
   call bar ( b3, lbound(b3,1), ubound(b3,1) )
   call bar ( c1, lbound(c1,1), ubound(c1,1) )
   call bar ( c2, lbound(c2,1), ubound(c2,1) )
   call bar ( c3, lbound(c3,1), ubound(c3,1) )

end program

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child

   class(base(4)), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(25) :: fmt

   write ( unit, * ) ' iotype:', iotype, ' v_list:', v_list

   select type ( dtv )
      type is ( base(4) )
         write ( fmt, * ) '(I', v_list(1),')'
         write ( unit, fmt, iostat = iostat )    dtv%i
      type is ( child(4,4) )
         write ( fmt, * ) '(I', v_list(1),', I', v_list(2),')'
         write ( unit, fmt, iostat = iostat )    dtv%i, dtv%j
   end select
   iomsg = 'dtiowrite'

end subroutine
