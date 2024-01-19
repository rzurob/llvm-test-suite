!*  ===================================================================
!*
!*  DATE                       : 2007-06-07 (original: 21/03/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        Test if compiler complains when char-literal-constant
!*                                        is other intrinsic types (integer, real, complex)
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

program fdtedit002dkl
use m

   class(base(4)), allocatable :: b1
   type(base(4))               :: b2 = base(4)(200)
   integer(4)               :: i1 = 0
   real(4)                  :: r1 = 10.0
   complex(4)               :: c1 = (20.0,30.0)

   integer :: stat
   character(150) :: msg

   open (1, file = 'fdtedit002dkl.1', form='formatted', access='sequential' )

   allocate ( b1, source = base(4)(100) )
   write ( 1, "(DTi1)" , iostat = stat, iomsg = msg )    b1
   write ( 1, "(DTr1)" , iostat = stat, iomsg = msg )    b1
   rewind 1
   read  ( 1, "(DTc1)" , iostat = stat, iomsg = msg )    b2
   read  ( 1, "(DT12345)" , iostat = stat, iomsg = msg ) b2

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
