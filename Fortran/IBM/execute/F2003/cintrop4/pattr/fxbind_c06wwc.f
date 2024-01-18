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
!*                              - test @PROCESS NOZEROSIZE.
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  :
!*   - Test: BINC(C) attribute with real array.
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

@PROCESS NOZEROSIZE

program fxbind_c06wwc
  use assertmod
  implicit none
  interface
     function check(x,n) bind(c)
       real(8) :: x(5)
       integer :: n
       real(8) :: check
     end function check
  end interface

  real(8):: s1(5), result
  integer :: n
  logical :: precision_R8,test
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
  test = precision_R8(result,15.0d0)
  call assert(test,'Hello, the result is not correct!',22)
end program fxbind_c06wwc

subroutine init_arr_1d(x,n)

  real(8)  :: x(5)
  integer  :: n
  do i = 1, n
     x(i) = real(i,8)
  end do

end subroutine init_arr_1d
