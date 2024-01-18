! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : isContigFalse6.f
!*
!*  PROGRAMMER                 : Dorra Bouchiha 
!*  DATE                       : 2010-10-25
!*  ORIGIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : IS_CONTIGUOUS intrinsic
!*                             :
!*  SECONDARY FUNCTIONS TESTED : ASSOCIATE and SELECTTYPE constructs 
!*
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : - Selector in ASSOCIATE construct or SELECTTYPE 
!*                                 construct is not contiguous
!*                               - IS_CONTIGUOUS should return .False.
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
MODULE Mod
      IMPLICIT NONE

      TYPE :: Base(K1,N1)  
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1

        INTEGER(K1)   :: BaseId = 1
      END TYPE
END MODULE
PROGRAM isContigFalse6
      USE Mod
      IMPLICIT NONE

      INTEGER :: Iarr(5,5)
      INTEGER :: V1(2)=[2,3]
      INTEGER, PARAMETER :: V2(2)=[1,1]
      TYPE(Base(4,10))  :: DT(5,5)
      CLASS(Base(4,:)), POINTER :: poly(:,:)

      Iarr = 1
      IF ( .NOT.  IS_CONTIGUOUS(Iarr) )       ERROR STOP 10
      IF ( IS_CONTIGUOUS(Iarr(V1,V1)) )       ERROR STOP 11
      IF ( IS_CONTIGUOUS(Iarr(V2,V2)) )       ERROR STOP 12
      IF ( IS_CONTIGUOUS(Iarr([1,1],[1,1])) ) ERROR STOP 13

      ASSOCIATE ( As => Iarr(:,:), As1 => Iarr([1,1],[1,1]), As2 => Iarr(V1,V1) )
          IF ( .NOT. IS_CONTIGUOUS(As) )      ERROR STOP 20
          IF (   IS_CONTIGUOUS(As1)    )      ERROR STOP 21
          IF (   IS_CONTIGUOUS(As2)    )      ERROR STOP 22
          ASSOCIATE ( As => As1(:,:), As1 => Iarr(V2,V2) )
              IF (   IS_CONTIGUOUS(As)  )     ERROR STOP 23
              IF (   IS_CONTIGUOUS(As1) )     ERROR STOP 24
          END ASSOCIATE
      END ASSOCIATE

      DT(1::1, 1::1) = Base(4,10)(BaseID=-2)
      IF ( .NOT.        IS_CONTIGUOUS(DT) )       ERROR STOP 30
      IF ( .NOT. IS_CONTIGUOUS(DT%BaseId) )       ERROR STOP 31
      IF (       IS_CONTIGUOUS(DT(V1,V1)) )       ERROR STOP 32
      IF (       IS_CONTIGUOUS(DT(V2,V2)) )       ERROR STOP 33
      IF ( IS_CONTIGUOUS(DT([1,1],[1,1])) )       ERROR STOP 34

      ASSOCIATE ( As => DT(:,:), As1 => DT%BaseId )
          IF ( .NOT.  IS_CONTIGUOUS(As) )      ERROR STOP 40
          IF ( .NOT. IS_CONTIGUOUS(As1) )      ERROR STOP 41
          ASSOCIATE ( As2 => As([1,1],[1,1]), As3 => As(V1,V1), As4 => As(V2,V2) )
              IF (   IS_CONTIGUOUS(As2)  )     ERROR STOP 42
              IF (   IS_CONTIGUOUS(As3)  )     ERROR STOP 43
              IF (   IS_CONTIGUOUS(As4)  )     ERROR STOP 44
          END ASSOCIATE
      END ASSOCIATE

      ALLOCATE( poly(5,5), SOURCE = DT )
      IF ( .NOT.        IS_CONTIGUOUS(poly) )  ERROR STOP 50
      IF ( .NOT. IS_CONTIGUOUS(poly%BaseId) )  ERROR STOP 51
      IF (       IS_CONTIGUOUS(poly(V1,V1)) )  ERROR STOP 52
      IF (       IS_CONTIGUOUS(poly(V2,V2)) )  ERROR STOP 53
      IF ( IS_CONTIGUOUS(poly([1,1],[1,1])) )  ERROR STOP 54

      SELECT TYPE ( s => poly(V1,V1) )
          TYPE IS (Base(4,*))
              IF ( IS_CONTIGUOUS(s) ) ERROR STOP 60

          CLASS DEFAULT
              ERROR STOP 61
      END SELECT
END PROGRAM isContigFalse6
