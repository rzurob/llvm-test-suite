! *********************************************************************
!**********************************************************************
!* ===================================================================
!*
!* DATE                         : Jan, 7, 2004
!*
!* PRIMARY FUNCTIONS TESTED     :Interoperable Functions.
!*                              - Test dummy procedure
!*                              - Fortran Code only
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  :
!*   - Test: Provide an explicit interface to the dummy procedure.
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  01/07/04   KT     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM test_integrate
  !
  !  Purpose:
  !    To test subroutine integrate, which integrates a function.
  !    The function to be integrated is passed to the subroutine
  !    as a calling argument.  This driver routine will integrate
  !    the function with step sizes dx of 0.001.
  !
  IMPLICIT NONE
  INTERFACE
     FUNCTION test_fun(x) BIND(C)
       REAL :: test_fun, x
     END FUNCTION test_fun
  END INTERFACE

  ! List of variables:
  REAL :: area   ! Area under the curve
  REAL  :: dx     ! Step size
  INTEGER :: error             ! Error flag
  INTEGER :: i                 ! Index variable
  REAL :: x1 = 0.              ! Starting point for integral.
  REAL :: x2 = 3.              ! Ending point for integral.

  LOGICAL :: precision_R4

  ! Initialize step sizes
  dx = 0.001

  ! Call subroutine "integrate" with each step size, and print
  ! out the results.
  DO i = 1, 7
     CALL integrate ( test_fun, x1, x2, dx, area, error )
  END DO

  ! Write out results.
  WRITE (*,1000)
1000 FORMAT ('0','  Step Size',5X,'Area',/, &
       ' ','  =========',5X,'====')

  WRITE (*,'(1X,F9.4,3X,F9.4)') dx, area
  if (.not. precision_R4(area,30.0)) then
     error stop 76
  endif
END PROGRAM test_integrate

REAL FUNCTION test_fun(x) BIND(C)
  !
  !  Purpose:dummy function
  !    Function to be integrated.
  !
  IMPLICIT NONE

  REAL, INTENT(IN) :: x

  ! Evaluate function.
  test_fun = 3 * x**2 + 1.0

END FUNCTION test_fun

SUBROUTINE integrate ( f, x1, x2, dx, area, error )
  !
  !  Purpose: user of dummy procedure argument.
  !    To integrate function f(x) between x1 and x2 using
  !    rectangles of width dx to approximate the area
  !    under the curve f(x).
  !
  IMPLICIT NONE

  ! Declare calling arguments:
  REAL, EXTERNAL :: f             ! Function to integrate
  REAL, INTENT(IN) :: x1          ! Starting point for integral
  REAL, INTENT(IN) :: x2          ! Ending point for integral
  REAL, INTENT(IN) :: dx          ! Step size
  REAL, INTENT(OUT) :: area       ! Area under the curve.
  INTEGER, INTENT(OUT) :: error   ! Error flag:
  !   0 = No error.
  !   1 = x1 > x2.

  ! Declare local variables:
  REAL :: height                  ! Height of rectangle
  INTEGER :: i                    ! Index variable
  INTEGER :: n                    ! Number of rectangles to integrate
  REAL :: width                   ! Width of rectangle
  REAL :: xstart                  ! Starting position of rectangle

  ! First, check to make sure that x1 <= x2.
  errchk: IF ( x1 > x2 ) THEN
     error = 1
  ELSE
     ! Clear error flag and area.
     error = 0
     area = 0

     ! Calculate the number of intervals to use.
     n = INT( (x2-x1) / dx + 1. )

     ! Calculate and sum the areas of each rectangle.
     sum: DO i = 1, n
        xstart = x1 + REAL(i-1) * dx
        width  = MIN ( dx, x2 - xstart )
        height = f( xstart + width/2. )
        area   = area + width * height
     END DO sum
  END IF errchk

END SUBROUTINE integrate
