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
!*                              - interop functions contained in Module.
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  :
!*   - Test: BINC(C) attribute with  different intrinsic data type,
!*           complex*4,complex*8.
!*
!*   - The interoperable  procedure itself is  implemented as C function.
!*   - The interoperabl Fortran procedure  has an explicit interface and
!*     is declared with the BIND attribute.
!*   - passing scalar arguments by REFERENCE and by VALUE
!*   - main written in FORTRAN, Fortran calls C functions.
!*   - Option : qrealsize=8
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

@PROCESS REALSIZE(8)
module mcom

  interface

     function fun_complex4_ref(x,y) BIND(C)
       complex   :: x
       complex   :: y
       complex   :: fun_complex4_ref
     end function fun_complex4_ref

     function fun_complex4_val(x,y) BIND(C)
       complex,value   :: x
       complex ,value   :: y
       complex   :: fun_complex4_ref
     end function fun_complex4_val

  end interface
end module mcom

program fxbind_c06aad
  use assertmod
  use  mcom
  complex  c4_ref, c4_val,ret4_ref,ret4_val
  complex(8)  c8_ref, c8_val

  !**********************************************************
  !        Initialization of variables                      *
  !**********************************************************
  c4_ref = (5.0d0,5.0d0)
  c8_ref = (10.0d0,10.0d0)

  c4_val = (5.0d0,5.0d0)
  c8_val = (10.0d0,10.0d0)

  !**********************************************************
  !        Calling C from Fortran with character data type
  !                and check the Results
  !**********************************************************

  ! Test 1 : call by reference
  ! A dummy argument without the VALUE attribute correspond
  ! to a formal parameter  of the prototype in C program
  ! that is of a pointer type.

  ret4_ref = fun_complex4_ref(c4_ref,c8_ref)
  if (  c4_ref /= (10.0d0,10.0d0) ) error stop 20
  if (  c8_ref /= (20.0d0,20.0d0) ) error stop 21

  ! Test 2 : call by value
  ! A dummy argument with the VALUE attribute  correspond
  ! to a formal parameter of the prototype in C program that is
  ! not of a pointer type.

  ret4_val= fun_complex4_val(c4_val,c8_val)
  if (  c4_val /= (5d0,5d0) ) error stop 22
  if (  c8_val /= (10.0d0,10.0d0) ) error stop 23

end program fxbind_c06aad
