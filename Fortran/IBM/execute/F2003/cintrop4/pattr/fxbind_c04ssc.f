! *********************************************************************
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
! *********************************************************************
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/runf.sh fxbind_c04ssc  
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
!* TEST CASE TITLE              : fxbind_c04ssc.f
!* TEST CASE TITLE              : BIND(C) for Fortran procedures 
!*                                contained in Module.
!* PROGRAMMER                   : Kan Tian
!* DATE                         : Jan, 7, 2004
!* ORIGIN                       : AIX Complier Development
!*                              : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     :Interoperable Functions.
!*                             
!* SECONDARY FUNTIONS TESTED
!*
!* DRIVER STANZA                : xlf95
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  :
!*   - Test: BINC(C) attribute with complex array.
!*   - FORTRAN code only , the interoperable function is implemented
!*     in Fortran and called in Fortran.
!*   - passing 1-dim complex  array arguments
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
     function   maxval_dc ( array, nvals, value_max) bind(c)
       INTEGER, INTENT(IN) :: nvals                     ! # vals.
       COMPLEX(16), INTENT(IN), DIMENSION(nvals) :: array   ! Input data.
       real(4), INTENT(OUT) :: value_max                ! Max value.
       INTEGER :: maxval_dc
     END  function   maxval_dc
  END INTERFACE
end module minfo

PROGRAM test_maxval
  ! The assert module is to test the truth of an assumption.
  use assertmod
  use  minfo
 
  LOGICAL :: test

  IMPLICIT NONE

  ! List of variables:
  COMPLEX(16), DIMENSION(6) :: array_dc           ! Real array

  REAL(4) :: value_max_r                     ! Max value

  INTEGER :: pos_maxval                       ! Pos of max value

  ! Initialize arrays

  array_dc = (/ (1._16,2._16), (-4._16,-6._16), &
       (4._16,-7._16), (3._16,4._16), &
       (0._16,1._16), (6._16,-8._16) /)

  ! Test real function.
  pos_maxval =  maxval_dc( array_dc, 6, value_max_r )

  test =  pos_maxval .EQ.  6
  call assert(test,'The result is not correct',9)

END PROGRAM test_maxval

function  maxval_dc ( array, nvals, value_max) bind(c)
  IMPLICIT NONE
  INTEGER, PARAMETER :: dbl = 16
  ! List of calling arguments:
  INTEGER, INTENT(IN) :: nvals                             
  COMPLEX(dbl), INTENT(IN), DIMENSION(nvals) :: array   
  REAL(4), INTENT(OUT) :: value_max                        
  INTEGER:: maxval_dc

  !  List of local variables:
  INTEGER :: i                            ! Index
  INTEGER :: pos_max                      ! Pos of max value

  ! Initialize the values to first value in array.
  value_max = ABS(array(1))
  pos_max = 1

  ! Find the extreme values in array(2) through array(nvals).
  DO i = 2, nvals
     IF ( ABS(array(i)) > value_max ) THEN
        value_max = ABS(array(i))
        pos_max = i
     END IF
  END DO

  ! Report the results

  maxval_dc = pos_max

END  function  maxval_dc
