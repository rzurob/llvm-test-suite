! *********************************************************************
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
! *********************************************************************
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/run.sh fxbind_c03wwa  cxbind_c03wwa
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
!* TEST CASE TITLE              : fxbind_c03wwa.f
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
!*   - Test: BINC(C) attribute with character array.
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

program fxbind_c03wwa
  use ISO_C_BINDING
  implicit none

  interface 

     function check(x,y) bind(c)
       use ISO_C_BINDING
       character :: x(5),y(5)
       logical(C_BOOL) :: check
     end function check

  end interface
  logical(C_BOOL) :: test
  character:: s1(5), s2(5)
  !**********************************************************
  !        Initialization of variables                      *
  !**********************************************************
  call init_arr_1d(s1,s2)

  !**********************************************************
  !        Calling C from Fortran with real data type
  !                and check the results
  !**********************************************************
  test = check(s1, s2)
 
end program fxbind_c03wwa

subroutine init_arr_1d(x,y)

  character :: x(5),y(5)

  do i = 1, 5
     x(i) = achar(iachar('A')+i-1)
  end do

  do i = 1, 5
     y(i) = achar(iachar('A')+i-1)
  end do

end subroutine init_arr_1d
