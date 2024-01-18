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
! %GROUP: deferred003.f
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
!*  DATE                       : 09/28/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing: Procedure Overridding
!*                                        deferred binding overridden by non-overridable procedure
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

type, abstract :: base(k1)
   integer, kind :: k1
   integer(k1) id
contains
   procedure(itf), pass, deferred :: getid   
end type

type, extends(base) :: child(k2)
   integer, kind :: k2
contains
   procedure, pass, non_overridable :: getid
end type

interface
   integer function itf(a)
      import base
      class(base(4)), intent(in) :: a
   end function
end interface

contains

   integer function getid(a)
      class(child(4,4)), intent(in) :: a
      getid = a%id
   end function
   
end module

program deferred003
   use m
   
   class(base(4)), allocatable :: b1
   
   allocate(b1, source = child(4,4)(10))
   if (b1%getid() .ne. 10) error stop 1_4   
   
end program
