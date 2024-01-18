! *********************************************************************
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
! *********************************************************************
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/run.sh fxbind_c03efe  cxbind_c03efe
! %COMPOPTS: 
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : fxbind_c03efe.f
!* TEST CASE TITLE              : BIND(C) for Fortran procedures 
!*
!* PROGRAMMER                   : Kan Tian
!* DATE                         : Jan, 7, 2004
!* ORIGIN                       : AIX Complier Development
!*                              : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     :Interoperable Functions.
!*                              - Fortran Entry in function called from C
!*            
!* SECONDARY FUNTIONS TESTED
!*
!* DRIVER STANZA                : xlf95
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  :
!*   - Test: BINC(C) attribute with  derived type.
!*   - The interoperable  procedure itself is implemented using Fortran 
!*     function Entry Statement.  
!*    - primary entry point have bind(c) attribute  and an alternate
!*     entry point do not have bind(c) attribute.
!*   - passing scalar arguments by REFERENCE and by VALUE
!*   - main written in C, C calls FORTRAN functions.
!*
!*  ALGORITHM :  
!*          1. C program call the Fortran function has a primary entry
!*             point and an alternate entry point.
!*          2. Assertion: Check the return value in C
!*             to verify it is correct.
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  01/07/04   KT     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

FUNCTION swap_drt(x,y) bind(c)
  use iso_c_binding
  type,bind(c):: ROBOT
     real   velocity 
     integer energy
     integer  IQ
     character name 
  end type ROBOT

  integer :: swap_drt,ent_swap_drt
  type(ROBOT):: x,y
  x =y 
  swap_drt=x%IQ
  return

  entry ent_swap_drt (x) 
  x%velocity = x%velocity + 10.0
  ent_swap_drt = x%IQ +20

  return
end FUNCTION swap_drt
