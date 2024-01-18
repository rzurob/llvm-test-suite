! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : associate002a1kl
!*
!*  DATE                       : 2007-06-29 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting with associate construct (Output)
!*                                        and change pointer targets
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

program associate002a1kl
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base(4)), allocatable, target :: b1
   class(base(4)), pointer     :: b2
   type(base(4)), allocatable, target  :: b4
   type(base(4)), pointer      :: b5

   namelist /nml/ b1, b2, b4, b5

   open (1, file = 'associate002a1kl.1', form='formatted', access='sequential' )
   allocate(b1,b2,b4,b5)

   b1%i = 2
   b2%i = 4
   b4%i = 8
   b5%i = 10

   write ( 1, nml, iostat = stat, iomsg = msg )

   associate ( b11 => b1 , b12 => b2, b13 => b3, b14 => b4, b15 => b5 )
      b2 => b1
      b5 => b4
      write ( 1, nml, iostat = stat, iomsg = msg )
   end associate

end program


subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base

   class(base(4)), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 4_4
   if ( size(v_list, 1) /= 0 ) error stop 5_4

   write (unit, "(' i=',I4,1X)", iostat=iostat )      dtv%i

   iomsg = 'dtiowrite'

end subroutine
