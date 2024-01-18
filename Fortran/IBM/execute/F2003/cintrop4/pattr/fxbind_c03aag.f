! *********************************************************************
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/run.sh fxbind_c03aag  cxbind_c03aag
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
!*
!* DATE                         : Jan, 7, 2004
!* ORIGIN                       : AIX Complier Development
!*
!* PRIMARY FUNCTIONS TESTED     :Interoperable Functions.
!*                              - Fortran programs interoperate with C functions
!*                                through a Fortran procedure interface that uses
!*                                the BIND specification .
!*                              - External Function with Bind(c) attribute,
!*                                Using Interface blocks to provide an
!*                                explicit interface
!*
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  :
!*   - Test: BINC(C) attribute with  different intrinsic data type,
!*           integer*1, integer*2, integer*4,integer*8.
!*   - The interoperable  procedure itself is  implemented as C function.
!*   - The interoperabl Fortran procedure  has an explicit interface and
!*     is declared with the BIND attribute.
!*   - passing scalar arguments by REFERENCE.
!*   - main written in FORTRAN, Fortran calls C functions.
!*   - Test typeless constants as function arguments.
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

program fxbind_c03aag
  use assertmod
  implicit none
  LOGICAL :: test

  interface
     function arith_int1_ref(x,y) BIND(C)
       integer*1 :: x,y
       integer*1 :: arith_int1_ref
     end function arith_int1_ref

     function arith_int2_ref(x,y) BIND(C)
       integer*2 :: x,y
       integer*2 :: arith_int2_ref
     end function arith_int2_ref

     function arith_int4_ref(x,y) BIND(C)
       integer*4 :: x,y
       integer*4 :: arith_int4_ref
     end function arith_int4_ref

     function arith_int8_ref(x,y) BIND(C)
       integer*8 :: x,y
       integer*8 :: arith_int8_ref
     end function arith_int8_ref

     function arith_int8_octal(x,y) BIND(C)
       integer*8 :: x,y
       integer*8 :: arith_int8_octal
     end function arith_int8_octal

  end interface

  !**********************************************************
  !        Initialization of variables                      *
  !**********************************************************

  integer*1 ai1_ref /5/, res1_ref
  integer*2 ai2_ref /5/, res2_ref
  integer*4 ai4_ref /5/, res4_ref
  integer*8 ai8_ref /5/, res8_ref, res9_ref

  !**********************************************************
  !        Calling C from Fortran with integer data type
  !                and check the results
  !**********************************************************

  ! Test 1 : call by reference
  ! A dummy argument without the VALUE attribute correspond
  ! to a formal parameter  of the prototype in C program
  ! that is of a pointer type.

  res1_ref = arith_int1_ref(ai1_ref,B'0001000')
  test =  ai1_ref .EQ. 10
  call assert(test,'Hello, the result is not correct!',20)
  test = res1_ref .EQ. 18
  call assert(test,'Hello, the result is not correct!',22)

  ! Test 2 : call by reference

  res2_ref = arith_int2_ref(ai2_ref,B'0001000')
  test =  ai2_ref .EQ. 10
  call assert(test,'Hello, the result is not correct!',26)
  test = res2_ref .EQ. 18
  call assert(test,'Hello, the result is not correct!',28)

  ! Test 3 : call by reference

  res4_ref = arith_int4_ref(ai4_ref,B'0001000')
  test =  ai4_ref .EQ. 10
  call assert(test,'Hello, the result is not correct!',32)
  test = res4_ref .EQ. 18
  call assert(test,'Hello, the result is not correct!',34)

  ! Test 4 : call by reference

  res8_ref = arith_int8_ref(ai8_ref,B'0001000')
  test =  ai8_ref .EQ. 10
  call assert(test,'Hello, the result is not correct!',38)
  test = res8_ref .EQ. 18
  call assert(test,'Hello, the result is not correct!',40)


  ! Test 5 : call by reference
  ! Re-initialize the argument value
  ai8_ref = 5

  res9_ref = arith_int8_octal(ai8_ref,O'777')
  test =  ai8_ref .EQ. 10
  call assert(test,'Hello, the result is not correct!',41)
  test = res9_ref .EQ. 521
  call assert(test,'Hello, the result is not correct!',42)

end program fxbind_c03aag
