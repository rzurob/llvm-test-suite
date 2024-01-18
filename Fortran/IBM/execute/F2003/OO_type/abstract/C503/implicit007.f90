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
! %GROUP: implicit007.f
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
!*  DESCRIPTION                : Testing: Abstract type with IMPLICIT STATEMENT
!*                                        unlimited polymorphic with implicit statement 
!*                                        and then allocated as (non-)polymorphic abstract
!*                                        type with non abstract dynamic type
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
   end type
   
   type, extends(base) :: child
   end type
   
end module

program implicit007
   use m  
   IMPLICIT class(*) (U-Z)
   pointer :: up1
   
   class(base), allocatable, target :: b1 
   class(base), pointer     :: b2
   
   allocate(b1, source = child(4))
   allocate(b2, source = child(5))
   
   up1=> b1
   
   if (.not. associated(up1, b1) ) error stop 1_4

   allocate(up1, source = b2)
   
   if ( associated(up1, b2) ) error stop 2_4
   
  
end program