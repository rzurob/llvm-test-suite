!#######################################################################
!************************************************************************
! %START
! %MAIN: YES
! %PRECMD:  $TR_SRC/run.sh fxmdvc16 cxmdvc16
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
!*
!*  TEST CASE NAME             : fxmdvc16.f
!*
!*  DATE                       : Sep 14,2002
!*
!*  PRIMARY FUNCTIONS TESTED   : Test 3 dimensions integer array variables,
!                                the combination of type and kind type
!                                parameter ,with bind(c) attribute/statement,
!                                is interoperate with  corresponding C type.
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*  DEPENDENCIES               : External routine ZZRC
!*
!*  KEYWORD(S)                 : C_INT_FAST8_T , DIMENSION
!*  DESCRIPTION                :
!*                              Pass data between a C variable with external
!*				linkage and Fortran variable has the bind(c)
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
  integer(C_INT_FAST8_T),dimension(0:5,0:4,0:3), bind(c) :: c3darray
end module mod

subroutine fsub()
  use mod
  integer(C_INT_FAST8_T),dimension(0:5,0:4,0:3) :: f3darray
  integer::i,j,k
  logical::res

  ! Initialize the array value used to compare with array passed
  ! from C program.
  DO i = 0, 3, 1
     Do j=0,4,1
        Do k=0,5,1
           f3darray(k,j,i)=3*(i+1)+2*(j+1)+k

        END DO
     END DO
  End DO

  ! Receives the value of  matrix c3darray from C program and verify
  ! the correctness.

  res=ALL(f3darray .eq. c3darray)

  if (res .neqv. .true.) then
     error stop 220
  endif

  print *, c3darray
  ! Change the array value,and the value will be checked in C program
  c3darray=c3darray *2
  print *, c3darray

end subroutine fsub
