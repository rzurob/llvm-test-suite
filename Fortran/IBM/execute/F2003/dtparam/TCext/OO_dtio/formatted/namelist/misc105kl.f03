! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-07-06 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Inside namelist formatting dtio, have namelist read and formatted read
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
      integer(kb) :: i,j,k
   end type

end module

program misc105kl
   use m

   integer :: stat
   character(150) :: msg = ''

   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(4)), intent(inout) :: dtv ! tcx: (4)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   class(base(4)), pointer    :: b1 ! tcx: (4)
   class(base(4)), pointer    :: b2 ! tcx: (4)
   type(base(4)), allocatable :: b3 ! tcx: (4)
   namelist /n1/ b1, b2, b3
   allocate(b1, b2, b3)

   open ( unit=1, file='misc105kl.1', access='sequential', form='formatted')

   read ( 1, n1, iostat = stat, iomsg = msg )

   if ( ( b1%i /= 5 )  .or. ( b1%j /= 6 )  .or. ( b1%k /= 7  ) )   error stop 1_4
   if ( ( b2%i /= 8 )  .or. ( b2%j /= 9 )  .or. ( b2%k /= 10 ) )   error stop 2_4
   if ( ( b3%i /= 11 ) .or. ( b3%j /= 12 ) .or. ( b3%k /= 13 ) )   error stop 3_4


end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base
   class(base(4)), intent(inout) :: dtv ! tcx: (4)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   integer :: dummy
   namelist /dtio/ dummy

   read ( unit, "(I4,1X)", iostat = iostat ) dtv%i
   read ( unit, dtio, iostat = iostat )
   read ( unit, "(I4)", iostat = iostat ) dtv%k

   dtv%j = dummy

   if ( iotype /= 'NAMELIST' ) error stop 4_4
   if ( size(v_list,1) /= 0 )  error stop 5_4

   iomsg = 'dtioread'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kb) to invoke with (4) / declare with (4) - 5 changes
