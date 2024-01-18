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
!*                              - External Function with Bind(c) attribute,
!*                                Using Interface blocks to provide an
!*                                explicit interface
!*                              - qintsize option (-qintsize = 4)
!*                                Check the @PROCESS INTSIZE  apply to
!*                                INTEGER statements with no  kind
!*                                specified.
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  :
!*   - Test: BINC(C) attribute with  different intrinsic data type,
!*           integer, integer*2, integer*4,integer*8.
!*   - The interoperable  procedure itself is  implemented as C function.
!*   - The interoperabl Fortran procedure  has an explicit interface and
!*     is declared with the BIND attribute.
!*   - passing scalar arguments by REFERENCE and by VALUE
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

program fxbind_c06aba
  use assertmod
  LOGICAL :: test
  implicit none

  interface
     function arith_int1_ref(x,y) BIND(C)
       integer :: x,y
       integer :: arith_int1_ref
     end function arith_int1_ref

     function arith_int1_val(x,y) BIND(C)
       integer,value :: x,y
       integer arith_int1_val
     end function  arith_int1_val

     function arith_int2_ref(x,y) BIND(C)
       integer*2 :: x,y
       integer*2 :: arith_int2_ref
     end function arith_int2_ref

     function arith_int2_val(x,y) BIND(C)
       integer*2,value :: x,y
       integer*2 arith_int2_val
     end function  arith_int2_val

     function arith_int4_ref(x,y) BIND(C)
       integer*4 :: x,y
       integer*4 :: arith_int4_ref
     end function arith_int4_ref

     function arith_int4_val(x,y) BIND(C)
       integer*4,value :: x,y
       integer*4 arith_int4_val
     end function  arith_int4_val

     function arith_int8_ref(x,y) BIND(C)
       integer*8 :: x,y
       integer*8 :: arith_int8_ref
     end function arith_int8_ref

     function arith_int8_val(x,y) BIND(C)
       integer*8,value :: x,y
       integer*8 arith_int8_val
     end function  arith_int8_val

  end interface

  !**********************************************************
  !        Initialization of variables                      *
  !**********************************************************

  integer ai1_ref /5/, bi1_ref /10/, res1_ref
  integer ai1_val /5/, bi1_val /10/, res1_val
  integer*2 ai2_ref /5/, bi2_ref /10/, res2_ref
  integer*2 ai2_val /5/, bi2_val /10/, res2_val
  integer*4 ai4_ref /5/, bi4_ref /10/, res4_ref
  integer*4 ai4_val /5/, bi4_val /10/, res4_val
  integer*8 ai8_ref /5/, bi8_ref /10/, res8_ref
  integer*8 ai8_val /5/, bi8_val /10/, res8_val

  !**********************************************************
  !        Calling C from Fortran with integer data type
  !                and check the results
  !**********************************************************

  ! Test 1 : call by reference
  ! A dummy argument without the VALUE attribute correspond
  ! to a formal parameter  of the prototype in C program
  ! that is of a pointer type.

  res1_ref = arith_int1_ref(ai1_ref,bi1_ref)
  test =  ai1_ref .EQ. 10
  call assert(test,'Hello, the result is not correct!',20)
  test =  bi1_ref .EQ. 20
  call assert(test,'Hello, the result is not correct!',21)
  test = res1_ref .EQ. 20
  call assert(test,'Hello, the result is not correct!',22)

  ! Test 2 : call by value
  ! A dummy argument with the VALUE attribute  correspond
  ! to a formal parameter of the prototype in C program that is
  ! not of a pointer type.

  res1_val =arith_int1_val(ai1_val,bi1_val)

  test =  ai1_val .EQ. 5
  call assert(test,'Hello, the result is not correct!',23)
  test =  bi1_val .EQ. 10
  call assert(test,'Hello, the result is not correct!',24)
  test = res1_val .EQ. 20
  call assert(test,'Hello, the result is not correct!',25)

  ! Test 3 : call by reference
  ! A dummy argument without the VALUE attribute correspond
  ! to a formal parameter  of the prototype in C program
  ! that is of a pointer type.

  res2_ref = arith_int2_ref(ai2_ref,bi2_ref)
  test =  ai2_ref .EQ. 10
  call assert(test,'Hello, the result is not correct!',26)
  test =  bi2_ref .EQ. 20
  call assert(test,'Hello, the result is not correct!',27)
  test = res2_ref .EQ. 20
  call assert(test,'Hello, the result is not correct!',28)

  ! Test 4 : call by value
  ! A dummy argument with the VALUE attribute  correspond
  ! to a formal parameter of the prototype in C program that is
  ! not of a pointer type.

  res2_val =arith_int2_val(ai2_val,bi2_val)
  test =  ai2_val .EQ. 5
  call assert(test,'Hello, the result is not correct!',29)
  test =  bi2_val .EQ. 10
  call assert(test,'Hello, the result is not correct!',30)
  test = res2_val .EQ. 20
  call assert(test,'Hello, the result is not correct!',31)

  ! Test 5 : call by reference
  ! A dummy argument without the VALUE attribute correspond
  ! to a formal parameter  of the prototype in C program
  ! that is of a pointer type.

  res4_ref = arith_int4_ref(ai4_ref,bi4_ref)

  test =  ai4_ref .EQ. 10
  call assert(test,'Hello, the result is not correct!',32)
  test =  bi4_ref .EQ. 20
  call assert(test,'Hello, the result is not correct!',33)
  test = res4_ref .EQ. 20
  call assert(test,'Hello, the result is not correct!',34)

  ! Test 6 : call by value
  ! A dummy argument with the VALUE attribute  correspond
  ! to a formal parameter of the prototype in C program that is
  ! not of a pointer type.

  res4_val =arith_int4_val(ai4_val,bi4_val)
  test =  ai4_val .EQ. 5
  call assert(test,'Hello, the result is not correct!',35)
  test =  bi4_val .EQ. 10
  call assert(test,'Hello, the result is not correct!',36)
  test = res4_val .EQ. 20
  call assert(test,'Hello, the result is not correct!',37)

  ! Test 7 : call by reference
  ! A dummy argument without the VALUE attribute correspond
  ! to a formal parameter  of the prototype in C program
  ! that is of a pointer type.

  res8_ref = arith_int8_ref(ai8_ref,bi8_ref)

  test =  ai8_ref .EQ. 10
  call assert(test,'Hello, the result is not correct!',38)
  test =  bi8_ref .EQ. 20
  call assert(test,'Hello, the result is not correct!',39)
  test = res8_ref .EQ. 20
  call assert(test,'Hello, the result is not correct!',40)

  ! Test 8 : call by value
  ! A dummy argument with the VALUE attribute  correspond
  ! to a formal parameter of the prototype in C program that is
  ! not of a pointer type.

  res8_val =arith_int8_val(ai8_val,bi8_val)
  test =  ai8_val .EQ. 5
  call assert(test,'Hello, the result is not correct!',41)
  test =  bi8_val .EQ. 10
  call assert(test,'Hello, the result is not correct!',42)
  test = res8_val .EQ. 20
  call assert(test,'Hello, the result is not correct!',43)

end program fxbind_c06aba
