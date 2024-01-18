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
! %GROUP: slash002.f
! %VERIFY:
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
!*  DESCRIPTION                : Testing: Section 10.10.1.2 Namelist Input Values
!*                                        When a slash is encountered, input statement should terminate.
!*                                        and any input data after the slash does NOT have to conform to rules of namelist input values
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
      integer(4)   :: i(3) = (/ -9, -9, -9 /)
      character(4) :: c(2)
   end type

end module

program slash002
   use m

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

   integer :: stat
   character(150) :: msg = ''

   class(base), allocatable  :: b1
   class(base), pointer      :: b2

   namelist /nml/ b1, b2
   allocate(b1, b2)
   open (1, file='slash002.1', form='formatted', access='sequential', blank='zero' )

   read (1, nml, iostat = stat, iomsg = msg)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   if ( ( b1%i(1) /= 101 ) .or. ( b1%i(2) /= 102 ) .or. ( b1%i(3) /= 103 ) .or. ( b1%c(1) /= 'abcd' ) .or. ( b1%c(2) /= 'ABCD' ) ) error stop 2_4
   if ( ( b2%i(1) /= 201 ) .or. ( b2%i(2) /= 202 ) .or. ( b2%i(3) /= 203 ) .or. ( b2%c(1) /= 'efgh' ) .or. ( b2%c(2) /= 'EFGH' ) ) error stop 3_4

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   integer(4) :: i(3)
   namelist /dtio/ i

   if ( iotype /= 'NAMELIST' )    error stop 4_4
   if ( size(v_list,1) /= 0 )     error stop 5_4

   read( unit, dtio, iostat = iostat )
   if ( iostat /= 0 )             error stop 6_4
   read( unit, "(A4,/,A4,/)", iostat = iostat )    dtv%c
   
   dtv%i = i

   iomsg = 'dtioread'

end subroutine
