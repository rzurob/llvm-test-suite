! *********************************************************************
!**********************************************************************
!* ===================================================================
!*
!* DATE                         : Jan 7, 2004
!*
!* PRIMARY FUNCTIONS TESTED     :Interoperable Functions.
!*                               array as arguments.
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  :
!*   - Test: Function with BINC(C) attribute.
!*   - The interoperable  procedure itself is implemented using
!*     external FORTRAN functions.
!*   - passing 1-dim  array arguments.
!*   - main written in C, C  calls FORTRAN functions.
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

function maxval_i2 (array, y) bind(c)
  IMPLICIT NONE
  integer(2),dimension(1:5):: array
  integer ::y
  INTEGER(2) ::  maxval_i2
  !  List of local variables:
  INTEGER :: i                            ! Index
  INTEGER(2):: value_max                ! Max value.

  ! Initialize the values to first value in array.
  value_max = array(1)
  print *," The value of array from C is :", array
  print *, " The value of y is:" ,y

  ! Find the extreme values in array.
  DO i = 2, y
     IF ( array(i) > value_max ) THEN
        value_max = array(i)
        print *, value_max
     END IF
  END DO

  ! Report the results

  maxval_i2 = value_max
  print *, value_max

END function  maxval_i2

function maxval_i4 (array, y) bind(c)
  IMPLICIT NONE
  integer(4),dimension(1:5):: array
  integer ::y
  INTEGER(4) ::  maxval_i4
  !  List of local variables:
  INTEGER :: i                            ! Index
  INTEGER(4):: value_max                ! Max value.

  ! Initialize the values to first value in array.
  value_max = array(1)
  print *," The value of array from C is :", array
  print *, " The value of y is:" ,y

  ! Find the extreme values in array.
  DO i = 2, y
     IF ( array(i) > value_max ) THEN
        value_max = array(i)
        print *, value_max
     END IF
  END DO

  ! Report the results

  maxval_i4 = value_max
  print *, value_max

END function  maxval_i4

function maxval_i8 (array, y) bind(c)
  IMPLICIT NONE
  integer(8),dimension(1:5):: array
  integer ::y
  INTEGER(8) ::  maxval_i8
  !  List of local variables:
  INTEGER :: i                            ! Index
  INTEGER(8):: value_max                ! Max value.

  ! Initialize the values to first value in array.
  value_max = array(1)
  print *," The value of array from C is :", array
  print *, " The value of y is:" ,y

  ! Find the extreme values in array.
  DO i = 2, y
     IF ( array(i) > value_max ) THEN
        value_max = array(i)
        print *, value_max
     END IF
  END DO

  ! Report the results

  maxval_i8 = value_max
  print *, value_max

END function  maxval_i8
