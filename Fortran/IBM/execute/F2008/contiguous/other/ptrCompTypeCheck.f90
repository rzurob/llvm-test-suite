! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : ptrCompTypeCheck.f
!*
!*  PROGRAMMER                 : Dorra Bouchiha 
!*  DATE                       : 2010-10-25
!*  ORIGIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : 
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : 
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
MODULE Mod1
      IMPLICIT NONE

      TYPE DT1  
        INTEGER, POINTER, CONTIGUOUS ::  ip(:)
      END TYPE DT1 

      TYPE DT2  
        REAL, POINTER, CONTIGUOUS ::  rp(:)
      END TYPE DT2 

      TYPE DT3  
        LOGICAL, POINTER, CONTIGUOUS :: lp(:)
      END TYPE DT3 

      TYPE DT4  
        COMPLEX, POINTER, CONTIGUOUS :: zp(:)
      END TYPE DT4 

      TYPE DT5
        CHARACTER(10),    POINTER, CONTIGUOUS :: cp(:)
      END TYPE DT5 

      TYPE DT6
        DOUBLE PRECISION, POINTER, CONTIGUOUS :: dp(:)
      END TYPE DT6 
END MODULE Mod1
PROGRAM ptrCompTypeCheck
      USE Mod1
      IMPLICIT TYPE (DT1) (i)
      IMPLICIT TYPE (DT2) (r)
      IMPLICIT TYPE (DT3) (l)
      IMPLICIT TYPE (DT4) (z)
      IMPLICIT TYPE (DT5) (c)
      IMPLICIT TYPE (DT6) (d)

      INTEGER, TARGET :: it(100) 
      REAL, TARGET :: rt(10) 
      LOGICAL, TARGET :: lt(2) 
      COMPLEX, TARGET :: zt(10) 
      CHARACTER(10), TARGET :: ct(10) 
      DOUBLE PRECISION, TARGET :: dt(10) 

      CLASS(*), POINTER :: poly(:)
 
      i%ip => it
      poly => i%ip
      IF ( .NOT. IS_CONTIGUOUS(poly) ) ERROR STOP 10

      r%rp => rt
      poly => r%rp
      IF ( .NOT. IS_CONTIGUOUS(poly) ) ERROR STOP 20

      l%lp => lt
      poly => l%lp
      IF ( .NOT. IS_CONTIGUOUS(poly) ) ERROR STOP 30

      z%zp => zt
      poly => z%zp
      IF ( .NOT. IS_CONTIGUOUS(poly) ) ERROR STOP 40

      c%cp => ct
      poly => c%cp
      IF ( .NOT. IS_CONTIGUOUS(poly) ) ERROR STOP 50

      d%dp => dt
      poly => d%dp
      IF ( .NOT. IS_CONTIGUOUS(poly) ) ERROR STOP 60
END PROGRAM ptrCompTypeCheck
