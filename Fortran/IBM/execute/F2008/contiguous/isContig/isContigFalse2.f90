! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-10-25
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   : IS_CONTIGUOUS intrinsic
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : -
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
PROGRAM isContigFalse2
      IMPLICIT NONE

      CHARACTER(10) :: carr(3)
      CHARACTER(10), ALLOCATABLE :: call(:)

      carr = ['AAAAAAAAAA', 'BBBBBBBBBB', 'CCCCCCCCCC']

      ALLOCATE(call(3), SOURCE = 'XLFtest')

      IF ( .NOT. IS_CONTIGUOUS(carr) )      ERROR STOP 10
      IF ( IS_CONTIGUOUS(carr(:)(1:2)) )    ERROR STOP 11
      IF ( IS_CONTIGUOUS(carr(1:2)(1:2)) )  ERROR STOP 12

      IF ( .NOT. IS_CONTIGUOUS(call) )      ERROR STOP 13
      IF ( IS_CONTIGUOUS(call(:)(1:2)) )    ERROR STOP 14
      IF ( IS_CONTIGUOUS(call(1:2)(1:2)) )  ERROR STOP 15
END PROGRAM isContigFalse2

