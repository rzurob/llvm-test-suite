!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : fdtedit006dkl
!*
!*  PROGRAMMER                 : David Forster (derived from fdtedit006d by Robert Ma)
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
!*                                        Test if compiler complains when v_list
!*                                        is selector of associate and select type construct
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

   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(4)), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

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

program fdtedit006dkl
use m

   class(base(4)), allocatable :: b1
   type(base(4))               :: b2  = base(4)(200)

   integer(4), parameter :: r1 = 1
   class(*), allocatable :: u1

   integer :: stat
   character(150) :: msg

   allocate ( u1, source = 5 )

   open (1, file = 'fdtedit006dkl.1', form='formatted', access='sequential' )

   allocate ( b1, source = base(4)(100) )

   associate ( g => r1 )
      write ( 1, "(DT(g))" , iostat = stat, iomsg = msg )     b1
   end associate

   select type ( u1 )
      type is (integer)
         write ( 1, "(DT(u1))" , iostat = stat, iomsg = msg ) b2
   end select

   rewind 1

   associate ( e => 1 )
      read ( 1, "(DT(e))" , iostat = stat, iomsg = msg )      b1
      associate ( g => e )
         read ( 1, "(DT(g))" , iostat = stat, iomsg = msg )   b2
      end associate
   end associate

end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base

   class(base(4)), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "DT" ) error stop 1_4
   if ( size(v_list, 1) /= 0 ) error stop 2_4

   error stop 3_4

   iomsg = 'dtioread'

end subroutine

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base

   class(base(4)), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "DT" ) error stop 4_4
   if ( size(v_list, 1) /= 0 ) error stop 5_4

   error stop 6_4

   iomsg = 'dtiowrite'

end subroutine
