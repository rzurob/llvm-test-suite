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
! %GROUP: input103.f
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

   type data
      integer(4)   :: i
      character(3) :: c
   end type

   type base
      type(data)   :: d1
      type(data)   :: d2
      integer(4)   :: j
      ! expanded into this order : base%d1%i, base%d1%c, base%d2%i, base%d2%c, base%j
   end type

end module

program input103
   use m

   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import data
         class(data), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   integer :: stat
   character(150) :: msg

   type(base) :: b1, b2
   namelist /nml/ b1, b2

   open (1, file='input103.1', form='formatted', access='sequential' )

   read (1, nml, iostat = stat, iomsg = msg)
   if ( ( b1%d1%i /= 1234 ) .or. ( b1%d1%c /= 'abc' ) .or. ( b1%d2%i /= 2345 ) .or. ( b1%d2%c /= 'def' ) .or. ( b1%j /= 3456 ) )  error stop 1_4
   if ( ( b2%d1%i /= 5678 ) .or. ( b2%d1%c /= 'ABC' ) .or. ( b2%d2%i /= 4567 ) .or. ( b2%d2%c /= 'DEF' ) .or. ( b2%j /= 7890 ) )  error stop 2_4

end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: data

   class(data), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   type(data) :: d1
   namelist /nml/ d1

   if ( iotype /= 'NAMELIST' ) error stop 3_4
   if ( size(v_list,1) /= 0 )  error stop 4_4

   read (unit, nml, iostat=iostat )

   dtv%i = d1%i
   dtv%c = d1%c

   iomsg = 'dtioread'

end subroutine

