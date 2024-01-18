! *********************************************************************
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
! *********************************************************************
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/run.sh fxbind_c12xxp  cxbind_c12xxp
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
!* TEST CASE TITLE              : fxbind_c12xxp.f
!* TEST CASE TITLE              : BIND(C) for Fortran procedures 
!*
!* PROGRAMMER                   : Kan Tian
!* DATE                         : Jan, 7, 2004
!* ORIGIN                       : AIX Complier Development
!*                              : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     :Interoperable Functions.
!*                              - Fortran programs interoperate with 
!*                                C functions through a Fortran procedure
!*                                that uses the BIND specification .
!*                              - Interoperable Pointers.
!*                                C_LOC, C_ASSOCIATED, C_PTR.
!*                                
!*            
!* SECONDARY FUNTIONS TESTED
!*
!* DRIVER STANZA                : xlf95
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

program m

  USE ISO_C_BINDING
  use assertmod

  interface
     function check(x,n) bind(c)
       USE ISO_C_BINDING
       IMPLICIT NONE
       TYPE (C_PTR),VALUE :: x
       integer:: n
       integer:: check
     end function check

  end interface
  logical:: test
  integer::  result,n
  !Declare Fortran variables to pass to a c function

  INTEGER, TARGET, DIMENSION(5) :: ALPHA
  TYPE(C_PTR) :: BETA
  n= 5 
  call init_arr_1d(ALPHA,n )
  BETA = C_NULL_PTR
  !If BETA is null, assign it the address of ALPHA
  IF (.NOT. C_ASSOCIATED(BETA)) THEN
     BETA = C_LOC(ALPHA)
  ENDIF

  result =  CHECK(BETA,n)
  test = result .eq. 15
  call assert(test,'Hello, the result is not correct!',22)

end program m

subroutine init_arr_1d(x,n)

  integer  :: x(5)
  integer  :: n
  do i = 1, n
     x(i) = i
  end do

end subroutine init_arr_1d
