! *********************************************************************
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
! *********************************************************************
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/rundat.sh fxbind_c04wwe  cxbind_c04wwe
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
!* TEST CASE TITLE              : fxbind_c04wwe.f
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
!*                              - Interop functions contained in Module.
!* SECONDARY FUNTIONS TESTED
!*
!* DRIVER STANZA                : xlf95
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  :
!*   - Test: Function with BINC(C) attribute and the argument is
!*     direved type  array.
!*   - The interoperable  procedure itself is implemented as C function.
!*   - The interoperabl Fortran procedure has an explicit interface and
!*     is declared with the BIND attribute.
!*   - main written in FORTRAN, Fortran calls C functions.
!*
!*  ALGORITHM :  
!*          1. Declare the interop functions in Fortran program.
!*          ( Create a procedural interface that corresponds to the C prototype
!*          and bind the interface to the C function using the BIND(C) specifier). 
!*          2. Declare the variable in Fortran which will be the  actual 
!*          arguments of the interop functions and Initialize the variables
!*          and do the calculation in C .
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

MODULE Personal_details
  IMPLICIT NONE
  TYPE, BIND(C):: Person
     REAL:: Weight
     INTEGER :: Age
     CHARACTER :: Sex
  END TYPE Person
END MODULE Personal_details

module mderinfo
  USE Personal_details
  implicit none
  interface
     function stats(x,n) BIND(C)
       USE Personal_details
       TYPE(Person) x(3)
       integer :: n
       real :: stats
     end function stats
  end interface
end module  mderinfo

program fxbind_c04wwe
  use assertmod
  use   mderinfo
  real :: result
  TYPE(Person)  info(3)
  TYPE(Person) :: compare = Person(50.0,18,'f')
  integer :: n
  logical :: precision_R4,test1, test
  !**********************************************************
  !        Initialization of variables                      *
  !**********************************************************
  n= 5 
  !**********************************************************
  !        Calling C from Fortran 
  !                and check the results
  !**********************************************************
  result = stats(info,n)
  test1 =  (compare.age  == info(1).age ) .AND. &
       (compare.Weight == info(1).Weight ) .AND. &
       (compare.Sex == info(1).Sex ) 
  
  test = test1 .AND. precision_R4(result,180.0e0)
  call assert(test,'Hello, the result is not correct!',22)
end program fxbind_c04wwe
