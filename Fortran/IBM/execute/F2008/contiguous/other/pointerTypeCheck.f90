! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : pointerTypeCheck.f
!*
!*  PROGRAMMER                 : Dorra Bouchiha 
!*  DATE                       : 2010-10-25
!*  ORIGIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Data pointer assingment 
!*                             :
!*  SECONDARY FUNCTIONS TESTED : Function result with CONTIGUOUS attribute
!*
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : - Pointer has CONTIGUOUS attribute 
!*    - Dummy is assumed shape array with or without CONTIGUOUS attribute
!*    - Actual is contiguous array 
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
PROGRAM pointerTypeCheck
      IMPLICIT NONE

      REAL,    POINTER, CONTIGUOUS :: rp(:)
      INTEGER, POINTER, CONTIGUOUS :: ip(:)
      LOGICAL, POINTER, CONTIGUOUS :: lp(:)
      COMPLEX, POINTER, CONTIGUOUS :: zp(:)
      CHARACTER(10),    POINTER, CONTIGUOUS :: cp(:)
      DOUBLE PRECISION, POINTER, CONTIGUOUS :: dp(:)
      CLASS(*), POINTER :: poly(:)

      ALLOCATE(ip(10)) 
      poly => ip
      IF ( .NOT. IS_CONTIGUOUS(poly) ) ERROR STOP 10

      ALLOCATE(rp(10)) 
      poly => rp
      IF ( .NOT. IS_CONTIGUOUS(poly) ) ERROR STOP 20

      ALLOCATE(lp(2)) 
      poly => lp
      IF ( .NOT. IS_CONTIGUOUS(poly) ) ERROR STOP 30

      ALLOCATE(cp(100)) 
      poly => cp
      IF ( .NOT. IS_CONTIGUOUS(poly) ) ERROR STOP 40

      ALLOCATE(zp(10)) 
      poly => zp
      IF ( .NOT. IS_CONTIGUOUS(poly) ) ERROR STOP 50

      ALLOCATE(dp(10)) 
      poly => dp
      IF ( .NOT. IS_CONTIGUOUS(poly) ) ERROR STOP 60

END PROGRAM pointerTypeCheck
