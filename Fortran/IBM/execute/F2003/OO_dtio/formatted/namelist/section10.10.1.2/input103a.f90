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
! %GROUP: input103a.f
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

   type innerdata
      character(3) :: c   = 'xxx'
   end type
   
   type data
      integer(4)   :: i = -999
      type(innerdata) :: i1
   end type

   type base
      integer(4)   :: j = -99
      type(data)   :: d1
      ! expanded into this order : base%j, base%d1%i, base%d1%i1%c
   end type

end module

program input103a
   use m

   integer :: stat
   character(150) :: msg

   type(base) :: b1
   type(base), allocatable :: b2
   namelist /nml/ b1, b2
   allocate(b2)

   open (1, file='input103a.1', form='formatted', access='sequential' )
   
   read (1, nml, iostat = stat, iomsg = msg)

   if ( ( b1%j /= 101 ) .or. ( b1%d1%i /= 1001 ) .or. ( b1%d1%i1%c /= 'abc' ) )  error stop 1_4
   if ( ( b2%j /= 202 ) .or. ( b2%d1%i /= 2002 ) .or. ( b2%d1%i1%c /= 'def' ) )  error stop 2_4

end program
