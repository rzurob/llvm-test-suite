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
!*   - Test: BINC(C) attribute with complex array.
!*   - using external FORTRAN functions
!*   - passing 1-dim  array arguments
!*   - main written in C
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  01/07/04   KT     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

!  Purpose:
!    returns the  maximum value in an array.

function maxval_c8 ( array, nvals) bind(c)
  IMPLICIT NONE
  ! Declare parameters:
  INTEGER, PARAMETER :: IB = 8
  ! List of calling arguments:
  INTEGER, INTENT(IN) :: nvals                     ! # vals.
  COMPLEX(IB), INTENT(IN), DIMENSION(nvals) :: array   ! Input data.

  INTEGER(4) ::  maxval_c8
  !  List of local variables:
  INTEGER :: i                            ! Index
  REAL(IB):: value_max                ! Max value.
  INTEGER :: pos_max    ! Position of max value
  ! Initialize the values to first value in array.
  value_max = ABS(array(1))

  pos_max = 0
  ! Find the extreme values in array(IB) through array(nvals).
  DO i = 2, nvals
     IF ( ABS(array(i)) > value_max ) THEN
        value_max = ABS(array(i))
        pos_max = i
     END IF
  END DO

  ! Report the results

  maxval_c8 = pos_max

END function  maxval_c8

function maxval_c4 ( array, nvals) bind(c)
  IMPLICIT NONE
  ! Declare parameters:
  INTEGER, PARAMETER :: IB = 4
  ! List of calling arguments:
  INTEGER, INTENT(IN) :: nvals                     ! # vals.
  COMPLEX(IB), INTENT(IN), DIMENSION(nvals) :: array   ! Input data.

  INTEGER(4) ::  maxval_c4
  !  List of local variables:
  INTEGER :: i                            ! Index
  REAL(IB):: value_max                ! Max value.
  INTEGER :: pos_max    ! Position of max value
  ! Initialize the values to first value in array.
  value_max = ABS(array(1))
  pos_max = 0

  ! Find the extreme values in array(IB) through array(nvals).
  DO i = 2, nvals
     IF ( ABS(array(i)) > value_max ) THEN
        value_max = ABS(array(i))
        pos_max = i
     END IF
  END DO

  ! Report the results

  maxval_c4 =  pos_max

END function  maxval_c4
