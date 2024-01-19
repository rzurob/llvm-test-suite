! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-06-29 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting with associate construct
!*                                        change value with associate-name (Output)
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

program associate002kl
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base(4)), allocatable :: b1
   type(base(4))               :: b3
   type(base(4)), allocatable  :: b4

   namelist /nml/ b1, b3, b4

   open (1, file = 'associate002kl.1', form='formatted', access='sequential' )
   allocate(b1, b4)

   b1%i = 2
   b3%i = 6
   b4%i = 8

   associate ( b11 => b1 , b13 => b3, b14 => b4 )

      b11%i = 4
      b13%i = 12
      b14%i = 16

      write (1,NML=nml, iostat=stat, iomsg=msg)

   end associate

   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

end program


subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base

   class(base(4)), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 2_4
   if ( size(v_list, 1) /= 0 ) error stop 3_4

   write (unit, "(' i=',I4,1X)", iostat=iostat )      dtv%i

   iomsg = 'dtiowrite'

end subroutine
