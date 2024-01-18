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
! %GROUP: interface003.f
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
!*  PROGRAMMER                 : Alberto Alvarez-Mesquida
!*  DATE                       : 02/20/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing: interface block
!*                                        poly abstract type return, interface of a deferred binding
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

type, abstract :: base
   integer :: id
contains
   procedure(itf), pass, deferred :: getbase   
end type

abstract interface
   class(base) function itf(a)
      import base
      class(base), intent(in) :: a
      pointer :: itf
   end function
end interface

type, extends(base) :: child
contains
   procedure, pass :: getbase
end type

contains

class(base) function getbase(a)
   class(child), intent(in) :: a
   pointer getbase
   allocate (getbase,source=a)
end function

end module

program abstracti020
   use m
   
   class(base), pointer :: b1
   type(child) :: c1 = child(5) 
   
   b1 => c1%getbase()
   
   if(b1%id .ne. 5) error stop 1_4
   
end program abstracti020
