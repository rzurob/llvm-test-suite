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
!*   - Test: BINC(C) attribute with intrinsic data type real*16.
!*
!*   - The interoperable  procedure itself is  implemented as C function.
!*   - The interoperabl Fortran procedure  has an explicit interface and
!*     is declared with the BIND attribute.
!*   - passing scalar arguments by REFERENCE and by VALUE
!*   - main written in FORTRAN, Fortran calls C functions.
!*   - Requires C compiler -qlongdbl option.
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

program fxbind_c03aak
  use assertmod
  implicit none
  LOGICAL :: test
  interface
     function arith_real16_ref(x,y) BIND(C)
       real*16 :: x,y
       real*16 :: arith_real16_ref
     end function arith_real16_ref

     function arith_real16_val(x,y) BIND(C)
       real*16,value :: x,y
       real*16 arith_real16_val
     end function  arith_real16_val
  end interface

  !**********************************************************
  !        Initialization of variables                      *
  !**********************************************************

  real*16 ai16_ref /5.0d0/, bi16_ref /10.0d0/, res16_ref
  real*16 ai16_val /5.0d0/, bi16_val /10.0d0/, res16_val

  !**********************************************************
  !        Calling C from Fortran with real data type
  !                and check the results
  !**********************************************************
  ! Test 1 : call by reference
  ! A dummy argument without the VALUE attribute correspond
  ! to a formal parameter  of the prototype in C program
  ! that is of a pointer type.

  test = .False.
  res16_ref = arith_real16_ref(ai16_ref,bi16_ref)
  test =  ai16_ref == 10.0d0
  call assert(test,'Hello, the result is not correct!',20)

  test =  bi16_ref == 20.0d0
  call assert(test,'Hello, the result is not correct!',21)

  test = res16_ref .eq. 20.0d0
  call assert(test,'Hello, the result is not correct!',22)

  ! Test 2 : call by value
  ! A dummy argument with the VALUE attribute  correspond
  ! to a formal parameter of the prototype in C program that is
  ! not of a pointer type.

  res16_val =arith_real16_val(ai16_val,bi16_val)
  test =  ai16_val == 5.0d0
  print *, "the ai16_val =", ai16_val
  call assert(test,'Hello, the result is not correct!',30)

  test =  bi16_val == 10.0d0
  call assert(test,'Hello, the result is not correct!',31)

  test = res16_val .eq. 20.0d0
  call assert(test,'Hello, the result is not correct!',32)

end program fxbind_c03aak
