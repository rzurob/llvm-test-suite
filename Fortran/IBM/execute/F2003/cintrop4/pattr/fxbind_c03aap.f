! *********************************************************************
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
! *********************************************************************
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/runcomplex16.sh fxbind_c03aap  cxbind_c03aap
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
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : fxbind_c03aap.f
!* TEST CASE TITLE              : BIND(C) for Fortran procedures 
!*
!* PROGRAMMER                   : Kan Tian
!* DATE                         : Jan, 7, 2004
!* ORIGIN                       : AIX Complier Development
!*                              : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     :Interoperable Functions.
!*                              - Fortran programs interoperate with C functions
!*                                through a Fortran procedure interface that uses
!*                                the BIND specification .
!*            
!* SECONDARY FUNTIONS TESTED
!*
!* DRIVER STANZA                : xlf95
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  :
!*   - Test: BINC(C) attribute with data type complex*16.
!*     
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

program fxbind_c03aap
  use assertmod
  LOGICAL :: test
  implicit none

  interface
     function fun_complex16_ref(x) BIND(C)
       complex(16)   :: x
       complex(16)   :: fun_complex16_ref
     end function fun_complex16_ref

     function fun_complex16_val(x) BIND(C)
       complex(16),value  :: x
       complex(16)  :: fun_complex16_val
     end function fun_complex16_val

  end interface

  complex(16) c16_ref, c16_val,ret16_ref,ret16_val

  !**********************************************************
  !        Initialization of variables                      *
  !**********************************************************

  c16_ref  = (5.0Q0,5.0Q0)
  c16_val  = (5.0Q0,5.0Q0)
  test = .False.
  !**********************************************************
  !        Calling C from Fortran with character data type
  !                and check the Results
  !**********************************************************
  ! Test 1 : call by reference
  ! A dummy argument without the VALUE attribute correspond 
  ! to a formal parameter  of the prototype in C program 
  ! that is of a pointer type.
  ret16_ref = fun_complex16_ref(c16_ref)
  print *,"ret16_ref =" , ret16_ref
  test =  ret16_ref == (10.0Q0,10.0Q0) 
  call assert(test,'Hello, the result is not correct!',20)

  test = c16_ref ==(10.0Q0,10.0Q0) 
  call assert(test,'Hello, the result is not correct!',21)

  ! Test 2 : call by value
  ! A dummy argument with the VALUE attribute  correspond
  ! to a formal parameter of the prototype in C program that is 
  ! not of a pointer type.
  ret16_val = (0.0Q0,0.0Q0)
  ret16_val = fun_complex16_val(c16_val)
  test =  ret16_val == (10.0Q0,10.0Q0) 
  print *, "ret16_val=" , ret16_val
  call assert(test,'Hello, the result is not correct!',22)
  
  test = c16_val ==(5.0Q0,5.0Q0) 
  call assert(test,'Hello, the result is not correct!',23)

  print *, "The testcase fxbind_c03aad.f is run successfully."

end program fxbind_c03aap
