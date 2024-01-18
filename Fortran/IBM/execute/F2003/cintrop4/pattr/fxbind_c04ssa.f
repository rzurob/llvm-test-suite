! *********************************************************************
!**********************************************************************
!* ===================================================================
!*
!*                                contained in Module.
!* DATE                         : Jan, 7, 2004
!*
!* PRIMARY FUNCTIONS TESTED     :Interoperable Functions.
!*
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  :
!*   - Test: BINC(C) attribute with integer array.
!*   - FORTRAN code only , the interoperable function is implemented
!*     in Fortran and called in Fortran.
!*   - passing 1-dim  integer array arguments
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  01/07/04   KT     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module minfo

 INTERFACE
     function   maxval_i ( array, nvals, value_max) bind(c)
       INTEGER, INTENT(IN) :: nvals                     ! # vals.
       INTEGER, INTENT(IN), DIMENSION(nvals) :: array   ! Input data.
       INTEGER, INTENT(OUT) :: value_max                ! Max value.
       INTEGER :: maxval_i
     END  function   maxval_i
  END INTERFACE

end module minfo

PROGRAM test_maxval
  ! The assert module is to test the truth of an assumption.
  use assertmod
  use  minfo
  LOGICAL :: test

  IMPLICIT NONE

  ! List of variables:
  INTEGER, DIMENSION(1:6) :: array_i            ! Integer array

  INTEGER :: value_max_i                      ! Max value

  INTEGER :: pos_maxval                       ! Pos of max value

  ! Initialize arrays
  array_i  = (/ -13,  3,  2,  0,  25,  -2  /)

  ! Test integer function.
  pos_maxval =  maxval_i( array_i, 6, value_max_i )

  test =  pos_maxval .EQ.  5
  call assert(test,'The result is not correct',9)

END PROGRAM test_maxval

function   maxval_i ( array, nvals, value_max) bind(c)
  IMPLICIT NONE

  ! List of calling arguments:
  INTEGER, INTENT(IN) :: nvals                     ! # vals.
  INTEGER, INTENT(IN), DIMENSION(nvals) :: array   ! Input data.
  INTEGER, INTENT(OUT) :: value_max                ! Max value.

  !  List of local variables:
  INTEGER :: i                            ! Index
  INTEGER :: pos_max                      ! Pos of max value
  INTEGER :: maxval_i
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

  maxval_i = pos_max

END function  maxval_i
