! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : parameter001kl
!*
!*  DATE                       : 2007-07-06 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting with dummy argument
!*                                        and associating with named-constant actual arg (output)
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

   type, extends(base) :: child (kc)
      integer, kind :: kc
      real(kc) :: r
   end type

   interface write(formatted)
      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(4)), intent(in) :: dtv ! tcx: (4)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   contains

   subroutine writeNamedConst ( unit, b1 )
      integer, intent(in) :: unit
      class(base(4)), intent(in) :: b1 ! tcx: (4)

      integer :: stat
      character(150) :: msg

      namelist /nml/ b1
      write (unit, nml, iostat = stat, iomsg = msg)

      if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   end subroutine

end module

program parameter001kl
use m

   integer :: stat
   character(200) :: msg = ''
   type(base(4)), parameter  :: b1 = base(4) (777) ! tcx: (4) ! tcx: (4)
   type(child(4,4)), parameter :: b2 = child(4,4)(888,8.88) ! tcx: (4,4) ! tcx: (4,4)

   open (1, file = 'parameter001kl.1', form='formatted', access='sequential' )

   call writeNamedConst(1, b1)
   call writeNamedConst(1, b2)

end program


subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child

   class(base(4)), intent(in) :: dtv ! tcx: (4)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 6_4
   if ( size(v_list, 1) /= 0 ) error stop 7_4

   write (unit, "('i=',I4,1X)", iostat=iostat )          dtv%i

   select type (dtv)
      type is (child(4,4)) ! tcx: (4,4)
         write (unit, "('r=',f8.3,1X)", iostat=iostat )  dtv%r
   end select

   iomsg = 'dtiowrite'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kb) to invoke with (4) / declare with (4) - 5 changes
! type: child - added parameters (kc) to invoke with (4,4) / declare with (4,4) - 3 changes
