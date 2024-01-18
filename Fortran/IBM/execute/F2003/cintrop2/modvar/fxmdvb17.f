!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!************************************************************************
! %START
! %MAIN: YES
! %PRECMD:  $TR_SRC/runint16.sh fxmdvb17 cxmdvb17 
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
!*  TEST CASE TITLE            : Support for  Module Variable with Bind(C)
!*                               (Test bind(c) attribute/statement)
!*
!*  PROGRAMMER                 : Kan Tian
!*  DATE                       : Sep 2,2002 
!*
!*  PRIMARY FUNCTIONS TESTED   : Test 2 dimensions integer array variables,
!                                the combination of type and kind type 
!                                parameter ,with bind(c) attribute/statement,
!                                is interoperate with  corresponding C type. 
!*                             
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  DRIVER STANZA              : xlf90
!*  REQUIRED COMPILER OPTIONS  :
!*  DEPENDENCIES               : External routine ZZRC
!*
!*  KEYWORD(S)                 : C_INT_FAST16_T , DIMENSION
!*  DESCRIPTION                :
!*                              Pass data between a C variable with external
!*				linkage and Fortran variable has the bind(c)
!*				attribute. 
!*                              Verify the result of data passing.
!*
!*  DESCRIPTION                :
!*                              Pass data between a C array with external
!*				linkage and Fortran array has the bind(c)
!*				attribute. 
!*                              Verify the result of data passing. 
!*                               
!*  ALGORITHM                  :  
!*          1. Declare the interop variable in Fortran Module.
!*          2. Initialize the variable in  C main program.
!*          3. C program call Fortran Subroutine to pass the variable value
!*             to Fortran.
!*          4. Assertion 1:  Check the value in Fortran to verify it
!*             is correct passed from C .
!*          5.	Modify the value in Fortran and pass back to C program
!*          6. Assertion 2: In C program ,Check the new value is correct
!*             passed from Fortran.
!*             if not, return the value other than "0" and program stops.
!*             otherwise, return "0" and program end.               
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  09/14/03    KT     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mod
  USE ISO_C_BINDING
  integer(C_INT_FAST16_T),dimension(0:4,0:3), bind(c) :: c2darray
end module mod

! Fortran Subroutine, will be called in C Program.
subroutine fsub()
  use mod
  integer(C_INT_FAST16_T),dimension(0:4,0:3) :: f2darray
  integer::i,j
  logical::res

  ! Initialize the value of matrix f2darry used to compare with
  ! c2darray, which is passed from C program.
  DO i = 0, 3, 1
     Do j=0,4,1
        f2darray(j,i)=10*(i+1)+j

     END DO
  End DO

  ! Receives the value of  matrix c2darray from C program and verify
  ! the correctness.

  res=ALL(f2darray .eq. c2darray)

  if (res .neqv. .true.) then
     error stop 220
  endif

  print *, c2darray
  ! Change the value of matrix c2darray and it will be checked in
  ! the C program.
  c2darray=c2darray *2
  print *, c2darray

end subroutine fsub

