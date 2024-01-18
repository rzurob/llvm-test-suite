! *********************************************************************
!**********************************************************************
!* ===================================================================
!*
!* DATE                         : Jan, 7, 2004
!*
!* PRIMARY FUNCTIONS TESTED     :Interoperable Functions.
!*
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  :
!*   - Test: BINC(C) attribute with real array.
!*   - FORTRAN code only , the interoperable function is implemented
!*     in Fortran and called in Fortran.
!*   - passing 1-dim real  array arguments
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  01/07/04   KT     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM test_maxval
  ! The assert module is to test the truth of an assumption.
  use assertmod
  INTERFACE
     function   maxval_r ( array, nvals, value_max) bind(c)
       INTEGER, INTENT(IN) :: nvals                     ! # vals.
       REAL(4), INTENT(IN), DIMENSION(nvals) :: array   ! Input data.
       REAL(4), INTENT(OUT) :: value_max                ! Max value.
       INTEGER :: maxval_r
     END  function   maxval_r
  END INTERFACE
  LOGICAL :: test

  IMPLICIT NONE

  ! List of variables:
  REAL(4), DIMENSION(6) :: array_r            ! Real array

  REAL(4) :: value_max_r                     ! Max value

  INTEGER :: pos_maxval                       ! Pos of max value

  ! Initialize arrays


array_r  = (/ -13., 3., 2., 0., 25., -2. /)

  ! Test real function.
  pos_maxval =  maxval_r( array_r, 6, value_max_r )

  test =  pos_maxval .EQ.  5
  call assert(test,'The result is not correct',9)

END PROGRAM test_maxval

function   maxval_r(array, nvals, value_max)
  IMPLICIT NONE

  ! List of calling arguments:
  INTEGER, INTENT(IN) :: nvals                     ! # vals.
  REAL(4), INTENT(IN), DIMENSION(nvals) :: array   ! Input data.
  REAL(4), INTENT(OUT) :: value_max                ! Max value.
  INTEGER :: maxval_r

  !  List of local variables:
  INTEGER :: i                            ! Index
  INTEGER :: pos_max                      ! Pos of max value

  ! Initialize the values to first value in array.
  value_max = array(1)
  pos_max = 1

  ! Find the extreme values in array(2) through array(nvals).
  DO i = 2, nvals
     IF ( array(i) > value_max ) THEN
        value_max = array(i)
        pos_max = i
     END IF
  END DO

  ! Report the results

  maxval_r = pos_max

END function  maxval_r
