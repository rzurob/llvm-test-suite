! *********************************************************************
!**********************************************************************
!* ===================================================================
!*
!* DATE                         : Jan, 7, 2004
!*
!* PRIMARY FUNCTIONS TESTED     :Interoperable procedure.
!*                              - Fortran programs interoperate with C functions
!*                                through a Fortran procedure interface that uses
!*                                the BIND specification .
!*                              - External subroutine with Bind(c) attribute,
!*                                Using Interface blocks to provide an
!*                                explicit interface
!*
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  :
!*   - Test: BINC(C) attribute with character argument.
!*   - The interoperable  procedure itself is  implemented as C function.
!*   - The interoperabl Fortran procedure  has an explicit interface and
!*     is declared with the BIND attribute.
!*   - passing scalar arguments by REFERENCE.
!*   - main written in FORTRAN, Fortran calls C functions.
!*
!*  ALGORITHM :
!*          1. Declare the interop functions in Fortran program.
!*          ( Create a procedural interface that corresponds to the C prototype
!*          and bind the interface to the C function using the BIND(C) specifier).
!*          2. Initialize the variable which will be the  actual arguments of
!*             the interop functions.
!*          3. Fortran  program call C function.The argument is  altered
!*             during execution of the C Function.
!*          4. Assertion: Check the modified auguments and return value
!*             in Fortran to verify it is correct.
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  01/07/04   KT     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM StringRefI
  use assertmod
  implicit none

  interface
     subroutine strrefi (x) bind(c)
       CHARACTER:: x(10)
     end subroutine  strrefi
  end interface

  CHARACTER:: a(10) , b(10)
  logical :: test
  call  init_arr_1d(a)
  call  init_arr_2d(b)

  CALL StrRefI(a)

  test = all ( a .eq. b)
  call assert(test,'Hello, the result is not correct!',22)

END PROGRAM StringRefI

subroutine init_arr_1d(x)
  character :: x(10)
  do i = 1, 10
     x(i) = achar(iachar('A')+i-1)
  end do
end subroutine init_arr_1d

subroutine init_arr_2d(x)
  character :: x(10)
  do i = 1, 10
     x(i) = achar(iachar('a')+i-1)
  end do
end subroutine init_arr_2d
