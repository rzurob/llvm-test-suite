! *********************************************************************
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
! *********************************************************************
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/run.sh fxbind_c03egc  cxbind_c03egc
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
!* TEST CASE TITLE              : fxbind_c03egc.f
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
!*   - Test: BINC(C) attribute with logical*1.
!*   - The interoperable  procedure itself is  implemented using Fortran 
!*     function Entry Statement.  
!*   - primary entry point do not have bind(c) attribute  and an alternate
!*     entry point have bind(c) attribute.
!*   - passing scalar arguments by REFERENCE and by VALUE
!*   - main written in C, C  calls FORTRAN functions.
!*
!*  ALGORITHM :  
!*          1. C program call the Fortran function has a primary entry
!*            point and an alternate entry point.
!*          2. Assertion: Check the return value  in C
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

function fn_log1 (a) 
  logical(1), intent(inout) :: a
  logical(1)             :: fn_log1
  logical(1)             :: ent_log1
  if ( a .neqv. .true. ) error stop 20
  a = .false.
  fn_log1 = .false.
  return
  entry ent_log1 () bind(c)
  ent_log1 = .true.
  return
end function fn_log1

function fn_logval1 (a) 
  logical(1), value :: a
  logical(1)             :: fn_logval1
  logical(1)             :: ent_logval1
  if ( a .neqv. .true. ) error stop 21
  a = .false.
  fn_logval1 = .false.
  return
  entry ent_logval1 () bind(c)
  ent_logval1 = .true.
  return
end function fn_logval1
