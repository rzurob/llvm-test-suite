! *********************************************************************
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/rundat.sh fxbind_c03wwe  cxbind_c03wwe
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
!*
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  :
!*   - Test: Function with BINC(C) attribute and the argument is
!*     derived type  array.
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

MODULE Personal_details

  IMPLICIT NONE
  TYPE, BIND(C):: Person
     REAL:: Weight
     INTEGER :: Age
     CHARACTER :: Sex
  END TYPE Person
END MODULE Personal_details

program fxbind_c03wwe
  use assertmod
  USE Personal_details
  implicit none

  interface

     function stats(x,n) bind(c)
       USE Personal_details
       TYPE(Person) x(3)
       integer :: n
       real :: stats
     end function stats

  end interface

  real :: result
  TYPE(Person)  info(3)
  TYPE(Person) :: compare = Person(50.0,18,'f')

  ! TYPE(Person):: info(3)
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

end program fxbind_c03wwe
