!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: parameter001.f
! %VERIFY: parameter001.1:parameter001.vf
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
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
   type base
      integer :: i
   end type

   type, extends(base) :: child
      real :: r
   end type

   interface write(formatted)
      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base), intent(in) :: dtv
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
      class(base), intent(in) :: b1

      integer :: stat
      character(150) :: msg

      namelist /nml/ b1
      write (unit, nml, iostat = stat, iomsg = msg)

      if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   end subroutine

end module

program parameter001
use m

   integer :: stat
   character(200) :: msg = ''
   type(base), parameter  :: b1 = base (777)
   type(child), parameter :: b2 = child(888,8.88)

   open (1, file = 'parameter001.1', form='formatted', access='sequential' )

   call writeNamedConst(1, b1)
   call writeNamedConst(1, b2)

end program


subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child

   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 6_4
   if ( size(v_list, 1) /= 0 ) error stop 7_4

   write (unit, "('i=',I4,1X)", iostat=iostat )          dtv%i

   select type (dtv)
      type is (child)
         write (unit, "('r=',f8.3,1X)", iostat=iostat )  dtv%r
   end select

   iomsg = 'dtiowrite'

end subroutine
