! *********************************************************************
!**********************************************************************
!* ===================================================================
!*
!* DATE                         : Jan, 7, 2004
!*
!* PRIMARY FUNCTIONS TESTED     :Interoperable Functions.
!*                              - Fortran programs interoperate with C functions
!*                                through a Fortran procedure interface that uses
!*                                the BIND specification .
!*
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  :
!*   - Test: BINC(C) attribute with integer array.
!*   - The interoperable  procedure itself is  implemented as C function.
!*   - The interoperabl Fortran procedure  has an explicit interface and
!*     is declared with the BIND attribute.
!*   - main written in FORTRAN, Fortran calls C functions.
!*
!*  ALGORITHM :
!*          1. Declare the interop functions in Fortran program.
!*          ( Create a procedural interface that corresponds to the C prototype
!*          and bind the interface to the C function using the BIND(C) specifier).
!*          2. Initialize the variable which will be the  actual arguments of
!*             the interop functions.
!*          3. Fortran  program call C function.
!*          4. Assertion: Check the return value from c function
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

program fxbind_c03wwb
  use assertmod
  implicit none
  interface
     function check(x,n) bind(c)
       integer:: x(5),n
       integer:: check
     end function check
  end interface

  logical:: test
  integer:: s1(5), result,n
  !**********************************************************
  !        Initialization of variables                      *
  !**********************************************************
  n= 5
  call init_arr_1d(s1,n )
  !**********************************************************
  !        Calling C from Fortran
  !                and check the results
  !**********************************************************
  result = check(s1,n)
  test = result .eq. 15
  call assert(test,'Hello, the result is not correct!',22)
end program fxbind_c03wwb

subroutine init_arr_1d(x,n)
  integer  :: x(5)
  integer  :: n
  do i = 1, n
     x(i) = i
  end do
end subroutine init_arr_1d
