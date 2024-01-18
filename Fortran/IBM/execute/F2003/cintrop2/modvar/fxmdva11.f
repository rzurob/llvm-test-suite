!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!************************************************************************
! %START
! %MAIN: YES
! %PRECMD:  $TR_SRC/run.sh fxmdva11 cxmdva11
! %COMPOPTS:  
! %GROUP: redherring.f 
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: 
! %END
!************************************************************************
!**********************************************************************
!*  ===================================================================
!*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxmdva11.f 
!*  TEST CASE TITLE            : Support for  Module Variable with Bind(C)
!*                               (Test bind(c) attribute/statement)
!*
!*  PROGRAMMER                 : Kan Tian
!*  DATE                       : Sep 2,2002 
!*
!*  PRIMARY FUNCTIONS TESTED   : Test integer variables,the combination
!*                               of type and kind type parameter ,with bind(c) 
!*                               attribute/statement, is interoperate with
!*                               corresponding C type. 
!* 
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  DRIVER STANZA              : xlf90
!*  REQUIRED COMPILER OPTIONS  :
!*  DEPENDENCIES               : External routine ZZRC
!*
!*  KEYWORD(S)                 : C_INT64_T
!*  DESCRIPTION                :
!*                              Pass data between a C variable with external
!*				linkage and Fortran variable has the bind(c)
!*				attribute. 
!*                              Verify the result of data passing.
!*
!*  TEST ITEMS                 :
!*          1.Test bind(c) attribute, declaring the binding label implicitly.
!*          2.Test bind(c) statement, declaring the binding label implicitly.
!*          3.Test bind(c) attribute, declaring the binding label explicitly.
!*          4.Test bind(c) statement, declaring the binding label explicitly.
!*          5.Declaration of variable in C code, use assignment statement assigns 
!*            a value to the variable in the main program.
!*          6.Declaration of a variable with its initialization in C code.
!*          7.Use of bind(c) statement before variable declaration.
!*          8.Use of bind(c) statement after  variable declaration.
!*
!*  ALGORITHM                  :  
!*          1. Declare the interop variable in Fortran Module.
!*          2. Initialize the variable in  C main program.
!*          3. C program call Fortran Subroutine to pass the variable value
!*             to Fortran.
!*          4. Assertion 1:  Check the value in Fortran to verify it
!*             is correct passed from C .
!*             If not, call ZZRC and program stops.
!*          5.	Modify the value in Fortran and pass back to C program
!*          6. Assertion 2: In C program ,Check the new value is correct
!*             passed from Fortran.
!*             if not, return the value other than "0" and program stops.
!*             otherwise, return "0" and program end.               
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  09/02/03    KT     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mod
  USE iso_c_binding
  ! the variables will be initialized in C.
  ! Declarations

  integer(C_INT64_T), bind(c) :: x1

  ! Use of bind(c) statement before variable declaration
  bind(c) :: x2
  integer(C_INT64_T) :: x2

  integer(C_INT64_T), bind(c,name="x3") :: fx3

  ! Use of bind(c) statement after  variable declaration
  integer(C_INT64_T) :: fx4
  bind(c,name="x4") :: fx4 


  INTERFACE assert_eq2                   ! generic name
     MODULE PROCEDURE assert4
  END INTERFACE

CONTAINS

  function assert4(n1,n2)
    INTEGER(C_INT64_T), INTENT(IN) :: n1,n2
    LOGICAL assert4
    if (n1 .NE. n2) then
       assert4= .FALSE.
    else
       assert4=.TRUE.
    endif
    return
  END function assert4

end module mod

subroutine fsub()
  use mod
  integer(C_INT64_T) :: basval1,basval2,basval3,basval4
  integer case_id
  LOGICAL ::result


  !*********************************************************************
  !  Testcase 1

  case_id = 1

  basval1 =1 
  result= assert_eq2(x1,basval1)
  print *, "In Fortran before changing:"
  print *, x1

  if (result .eqv. .FALSE.) then
     call zzrc(case_id)
  endif

  x1 = x1 + 1
  print *, "In Fortran after changing:"
  print *, x1

  !*********************************************************************
  !  Testcase 2

  case_id = 2

  basval2 =1 
  result= assert_eq2(x2,basval2)
  print *, "In Fortran before changing:"
  print *, x2

  if (result .eqv. .FALSE.) then
     call zzrc(case_id)
  endif

  x2 = x2 + 1
  print *, "In Fortran after changing:"
  print *, x2

  !*********************************************************************
  !  Testcase 3

  case_id = 3

  basval3 =1 
  result= assert_eq2(fx3,basval3)
  print *, "In Fortran before changing:"
  print *, fx3

  if (result .eqv. .FALSE.) then
     call zzrc(case_id)
  endif

  fx3 = fx3 + 1
  print *, "In Fortran after changing:"
  print *, fx3


  !*********************************************************************
  !  Testcase 4

  case_id = 4

  basval4 =1 
  result= assert_eq2(fx4,basval4)
  print *, "In Fortran before changing:"
  print *, fx4

  if (result .eqv. .FALSE.) then
     call zzrc(case_id)
  endif

  fx4 = fx4 + 1
  print *, "In Fortran after changing:"
  print *, fx4

end subroutine fsub

