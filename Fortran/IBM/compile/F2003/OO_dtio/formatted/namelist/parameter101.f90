!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: parameter101.f
! %VERIFY: parameter101.1:parameter101.vf
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/08/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting with dummy argument
!*                                        and associating with named-constant actual arg (input)
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

   procedure(logical) :: precision_r4

   contains

   subroutine readNamedConst ( unit, b1 )
      integer, intent(in) :: unit
      class(base), intent(inout) :: b1

      integer :: stat
      character(150) :: msg

      namelist /nml/ b1
      read (unit, nml, iostat = stat, iomsg = msg)
      if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

      select type ( b1 )
         type is (base)
            if ( b1%i /= 111 ) error stop 2_4
         type is (child)
            if ( ( b1%i /= 111 ) .or. ( .not. precision_r4(b1%r,11.11) ))  error stop 3_4
         class default
            error stop 4_4
      end select

   end subroutine

end module

program parameter101
use m

   integer :: stat
   character(200) :: msg = ''
   type(base), parameter  :: b1 = base (777)
   type(child), parameter :: b2 = child(888,8.88)

   open (1, file = 'parameter101.1', form='formatted', access='sequential' )

   call readNamedConst(1, b1)
   call readNamedConst(1, b2)

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 6_4
   if ( size(v_list, 1) /= 0 ) error stop 7_4

   read (unit, "(1X,I4)", iostat=iostat )          dtv%i

   select type (dtv)
      type is (child)
         read (unit, "(1X,f8.3)", iostat=iostat )  dtv%r
   end select

   iomsg = 'dtioread'

end subroutine
