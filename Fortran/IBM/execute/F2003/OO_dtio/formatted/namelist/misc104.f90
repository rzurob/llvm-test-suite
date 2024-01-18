!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: misc104.f
! %VERIFY:
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
!*  DESCRIPTION                : Testing: Both pointer and pointer targets in the same namelist
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
      integer(4)   :: i
   end type

   type, extends(base) :: child
      real(4)      :: r
   end type

end module

program misc104
   use m

   integer :: stat
   character(150) :: msg = ''
   procedure (logical) :: precision_r4

   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   class(base), pointer :: b1
   class(child), pointer :: c1
   class(child), allocatable, target :: c2

   namelist /nml/ c2, c1, b1  !<- in namelist data, c1 is read last. therefore, all c2, c1, b1 shall have the values of c1

   allocate ( c2, source= child(r= -9.9999, i = -9999) )
   open (1, file='misc104.1', form='formatted', access='sequential' )

   c1 => c2
   b1 => c1

   read (1, nml, iostat = stat, iomsg = msg)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4
   select type (b1)
      type is (child)
         if ( ( b1%i /= 9 ) .or. ( .not. precision_r4(b1%r, -9.0) ) ) error stop 1_4
         if ( ( c1%i /= 9 ) .or. ( .not. precision_r4(c1%r, -9.0) ) ) error stop 2_4
         if ( ( c2%i /= 9 ) .or. ( .not. precision_r4(c2%r, -9.0) ) ) error stop 3_4
   end select

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child
   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= 'NAMELIST' ) error stop 4_4
   if ( size(v_list,1) /= 0 )  error stop 5_4

   select type(dtv)
      type is (base)
         read(unit, *, iostat = iostat) dtv%i
      type is (child)
         read(unit, *, iostat = iostat) dtv%i, dtv%r
      class default
         error stop 4_4
   end select

   iomsg = 'dtioread'

end subroutine
