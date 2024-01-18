!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArg001kl
!*
!*  PROGRAMMER                 : David Forster (derived from dummyArg001 by Robert Ma)
!*  DATE                       : 2007-06-01 (original: 21/03/2005)
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
!*                                        Argument Association: non-polymorphic scalar dummy argument
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

   type base (k)
      integer, kind :: k
      integer(k) :: i = -999
   end type

   type, extends(base) :: child (k2)
   integer, kind :: k2
      integer(k2) :: j = -999
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
         type(base(4)), intent(in) :: dtv
         character(13) :: fmt = "(DT'base'(5))"

         write (1, fmt, iostat = stat, iomsg = msg ) dtv
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) )  error stop 1_4

      end subroutine

      subroutine bar ( dtv )
         type(child(4,4)), intent(inout) :: dtv
      10 format (DT'child'(8,9))

         write (1, 10, iostat = stat, iomsg = msg ) dtv

         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) )  error stop 2_4

      end subroutine

end module

program dummyArg001kl
use m

   type(base(4)), allocatable :: b1
   type(base(4)), pointer     :: b2
   type(base(4))              :: b3 = base(4) ( 103 )

   type(child(4,4)), allocatable :: c1
   type(child(4,4)), pointer     :: c2
   type(child(4,4))              :: c3 = child(4,4) ( 203, 213 )

   open (1, file = 'dummyArg001kl.1', form='formatted', access='sequential' )

   allocate ( b1, source = base(4)(101) )
   allocate ( b2, source = base(4)(102) )
   allocate ( c1, source = child(4,4)(201, 211) )
   allocate ( c2, source = child(4,4)(202, 212) )

   call foo ( b1 )
   call foo ( b2 )
   call foo ( b3 )

   call bar ( c1 )
   call bar ( c2 )
   call bar ( c3 )

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
