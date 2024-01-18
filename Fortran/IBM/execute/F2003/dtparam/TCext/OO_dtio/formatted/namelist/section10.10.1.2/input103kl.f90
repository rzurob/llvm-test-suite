! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : input103kl
!*
!*  DATE                       : 2007-07-20 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.10.1.1 Namelist Input Values
!*                                        Derived type variable shall be expanded into intrinsic types
!*                                       (no dtio procedure involved, with polymorphic component)
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

   type data (kd,ld) ! kd,ld=4,3
      integer, kind :: kd
      integer, len :: ld
      integer(kd)   :: i
      character(ld) :: c
   end type

   type base (kb,lb) ! kb,lb=4,3
      integer, kind :: kb
      integer, len :: lb
      type(data(kb,lb))   :: d1 ! tcx: (kb,lb)
      type(data(kb,lb))   :: d2 ! tcx: (kb,lb)
      integer(kb)   :: j
      ! expanded into this order : base%d1%i, base%d1%c, base%d2%i, base%d2%c, base%j
   end type

end module

program input103kl
   use m

   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import data
         class(data(4,*)), intent(inout) :: dtv ! tcx: (4,*)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   integer :: stat
   character(150) :: msg

   type(base(4,3)) :: b1, b2 ! tcx: (4,3)
   namelist /nml/ b1, b2

   open (1, file='input103kl.1', form='formatted', access='sequential' )

   read (1, nml, iostat = stat, iomsg = msg)
   if ( ( b1%d1%i /= 1234 ) .or. ( b1%d1%c /= 'abc' ) .or. ( b1%d2%i /= 2345 ) .or. ( b1%d2%c /= 'def' ) .or. ( b1%j /= 3456 ) )  error stop 1_4
   if ( ( b2%d1%i /= 5678 ) .or. ( b2%d1%c /= 'ABC' ) .or. ( b2%d2%i /= 4567 ) .or. ( b2%d2%c /= 'DEF' ) .or. ( b2%j /= 7890 ) )  error stop 2_4

end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: data

   class(data(4,*)), intent(inout) :: dtv ! tcx: (4,*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   type(data(4,3)) :: d1 ! tcx: (4,3)
   namelist /nml/ d1

   if ( iotype /= 'NAMELIST' ) error stop 3_4
   if ( size(v_list,1) /= 0 )  error stop 4_4

   read (unit, nml, iostat=iostat )

   dtv%i = d1%i
   dtv%c = d1%c

   iomsg = 'dtioread'

end subroutine



! Extensions to introduce derived type parameters:
! type: data - added parameters (kd,ld) to invoke with (4,3) / declare with (4,*) - 5 changes
! type: base - added parameters (kb,lb) to invoke with (4,3) / declare with (4,*) - 1 changes
