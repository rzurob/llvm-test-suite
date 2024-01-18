! *********************************************************************
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
! *********************************************************************
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/run.sh fxbind_c04aab  cxbind_c04aab
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
!* TEST CASE TITLE              : fxbind_c04aab.f
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
!*                              - interop functions contained in Module.
!*              
!* SECONDARY FUNTIONS TESTED
!*
!* DRIVER STANZA                : xlf95
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  :
!*   - Test: BINC(C) attribute with  different intrinsic data type,
!*           real*4,real*8.
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
module mreal
  interface
     function arith_real4_ref(x,y) BIND(C)
       real*4 :: x,y
       real*4 :: arith_real4_ref
     end function arith_real4_ref

     function arith_real4_val(x,y) BIND(C)
       real*4,value :: x,y
       real*4 arith_real4_val
     end function  arith_real4_val

     function arith_real8_ref(x,y) BIND(C)
       real*8 :: x,y
       real*8 :: arith_real8_ref
     end function arith_real8_ref

     function arith_real8_val(x,y) BIND(C)
       real*8,value :: x,y
       real*8 arith_real8_val
     end function  arith_real8_val
    
  end interface
end module mreal

program fxbind_c04aab

  use mreal
  implicit none

  !**********************************************************
  !        Initialization of variables                      *
  !**********************************************************

  real*4 ai4_ref /5.0e0/, bi4_ref /10.0e0/, res4_ref
  real*4 ai4_val /5.0e0/, bi4_val /10.0e0/, res4_val
  real*8 ai8_ref /5.0d0/, bi8_ref /10.0d0/, res8_ref
  real*8 ai8_val /5.0d0/, bi8_val /10.0d0/, res8_val
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

  res4_ref = arith_real4_ref(ai4_ref,bi4_ref)
  if ( ai4_ref /= 10.0e0 ) error stop 32
  if ( bi4_ref /= 20.0e0 ) error stop 33
  if(res4_ref .ne. 20.0e0)then
     error stop 34
  endif

  ! Test 2 : call by value
  ! A dummy argument with the VALUE attribute  correspond
  ! to a formal parameter of the prototype in C program that is 
  ! not of a pointer type.

  res4_val =arith_real4_val(ai4_val,bi4_val)
  if ( ai4_val /= 5.0e0 ) error stop 35
  if ( bi4_val /= 10.0e0 ) error stop 36
  if(res4_val .ne. 20.0e0)then
     error stop 37
  endif

  ! Test 3 : call by reference
  ! A dummy argument without the VALUE attribute correspond 
  ! to a formal parameter  of the prototype in C program 
  ! that is of a pointer type.

  res8_ref = arith_real8_ref(ai8_ref,bi8_ref)
  if ( ai8_ref /= 10.0d0 ) error stop 38
  if ( bi8_ref /= 20.0d0 ) error stop 39
  if(res8_ref .ne. 20.0d0)then
     error stop 40
  endif

  ! Test 4 : call by value
  ! A dummy argument with the VALUE attribute  correspond
  ! to a formal parameter of the prototype in C program that is 
  ! not of a pointer type.

  res8_val =arith_real8_val(ai8_val,bi8_val)
  if ( ai8_val /= 5.0d0 ) error stop 41
  if ( bi8_val /= 10.0d0 ) error stop 42
  if(res8_val .ne. 20.0d0)then
     error stop 43
  endif

end program fxbind_c04aab
