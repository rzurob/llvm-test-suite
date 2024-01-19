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
!*                                        Try namelist formatting with associate construct (Output)
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
      integer(kb), pointer :: i
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

end module

program associate102kl
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base(4)), allocatable :: b1
   integer, target :: i1, i2, i3

   namelist /nml/ b1

   open (1, file = 'associate102kl.1', form='formatted', access='sequential' )
   allocate(b1)

   i1 = -999
   i2 = -999
   i3 = -999

   b1%i => i1

   associate ( b11 => b1 )
      read (1,NML=nml, iostat=stat, iomsg=msg)
      if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4
      if (( b11%i /= 1 ) .or. ( b1%i /=1 ) .or. ( i1 /= 1 ) ) error stop 2_4

      b11%i => i2
      read (1,NML=nml, iostat=stat, iomsg=msg)
      if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 3_4
      if (( b11%i /= 2 ) .or. ( b1%i /=2 ) .or. ( i2 /= 2 ) ) error stop 4_4

      b11%i => i3
      read (1,NML=nml, iostat=stat, iomsg=msg)
      if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 5_4
      if (( b11%i /= 3 ) .or. ( b1%i /=3 ) .or. ( i3 /= 3 ) ) error stop 6_4

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

   if ( iotype /= "NAMELIST" ) error stop 7_4
   if ( size(v_list, 1) /= 0 ) error stop 8_4

   read (unit, "(I1)", iostat=iostat )      dtv%i

   iomsg = 'dtioread'

end subroutine
