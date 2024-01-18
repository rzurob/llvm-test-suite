!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArg005kl
!*
!*  PROGRAMMER                 : David Forster (derived from dummyArg005 by Robert Ma)
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
!*                                        Argument Association: assumed size array dummy argument
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
         type(base(4)), intent(in) :: dtv(2,*)
         character(27) :: fmt 
         
         fmt = "(DT'_foo'(5),/,DT'_foo'(6))"

         write (1, fmt, iostat = stat, iomsg = msg ) dtv(1:2,1:2)

         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) )  error stop 1_4

      end subroutine

      subroutine bar ( dtv )
         class(base(4)), intent(in) :: dtv(3:4,*)
      10 format (DT'_bar'(4,5),/,DT'_bar'(5,6),/,DT'_bar'(6,7),/,DT'_bar'(7,8))

         write (1, 10, iostat = stat, iomsg = msg ) dtv(3:4,1:2)

         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) )  error stop 2_4

      end subroutine

end module

program dummyArg005kl
use m

   type(base(4)), allocatable  :: b1(:,:)
   type(base(4))               :: b2(4) = (/ base(4)(111), base(4)(112), base(4)(113), base(4)(114) /)
   class(base(4)), pointer     :: b3(:,:)

   type(child(4,4)), allocatable :: c1(:,:)
   type(child(4,4))              :: c2(3:6) = (/ child(4,4)(211,212), child(4,4)(213,214), child(4,4)(215,216), child(4,4)(217, 218) /)
   class(child(4,4)), pointer    :: c3(:)

   open (1, file = 'dummyArg005kl.1', form='formatted', access='sequential' )

   allocate ( b1(2,2), source = reshape ( source = (/ base(4)(101), base(4)(102), base(4)(103), base(4)(104) /), shape = (/2,2/) ) )
   allocate ( b3(2,2), source = reshape ( source = (/ child(4,4)(121,122), child(4,4)(123,124), child(4,4)(125,126), child(4,4)(127,128) /), shape = (/2,2/) ) )

   allocate ( c1(2,2), source = reshape ( source = (/ child(4,4)(201,202), child(4,4)(203,204), child(4,4)(205,206), child(4,4)(207,208) /), shape = (/ 2, 2 /) ) )
   allocate ( c3(4:7), source = (/ child(4,4)(221,222), child(4,4)(223,224), child(4,4)(225,226), child(4,4)(227, 228) /) )

   call foo ( b1 )
   call foo ( b2((/1,1,1,1/)) )
   call foo ( b3 )
   call bar ( b1 )
   call bar ( b2 )
   call bar ( b3 )
   call bar ( c1(1:2,1:2) )
   call bar ( c2(3:6) )
   call bar ( c3((/4,5,6,7/)) )

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
