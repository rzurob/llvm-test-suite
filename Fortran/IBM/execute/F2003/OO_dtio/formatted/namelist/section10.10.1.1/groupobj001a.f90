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
! %GROUP: groupobj001a.f
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
!*  DESCRIPTION                : Testing: Section 10.10.1.1 Namelist group object names
!*                                        Input data being object components of sequential type entities
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
      sequence
      character(4) :: c = 'xxxx'
      integer(4)   :: i = 999
      real(4)      :: r = -9.0
   end type

end module

program groupobj001a
   use m
   implicit type(base) (x-z)
   integer :: stat
   character(200) :: msg
   type(base), allocatable :: b1
   type(base), pointer     :: b2
   type(base)              :: b3
   procedure(logical) :: precision_r4
   namelist /nml/ b1, b2, b3, x1

   allocate ( b1 , b2 )

   open (1, file='groupobj001a.1', form='formatted', access='sequential' )

   read (1, nml, iostat = stat, iomsg = msg)

   if ( ( b1%i /= 999  ) .or. ( b1%c /= 'abcd' ) .or. ( .not. precision_r4( b1%r, -9.0 ) ) ) error stop 1_4
   if ( ( b2%i /= 2002 ) .or. ( b2%c /= 'efgh' ) .or. ( .not. precision_r4( b2%r, 1.0  ) ) ) error stop 2_4
   if ( ( b3%i /= 3003 ) .or. ( b3%c /= 'ijkl' ) .or. ( .not. precision_r4( b3%r, -9.0 ) ) ) error stop 3_4
   if ( ( x1%i /= 4004 ) .or. ( x1%c /= ''     ) .or. ( .not. precision_r4( x1%r, 2.0  ) ) ) error stop 4_4

end program
