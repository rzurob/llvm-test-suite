! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp /tstdev/OO_poly/selectType/Quotes/C811ArrVecAssoc1.f
! opt variations: -qnok -qnol -qnodeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 2, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Constraint C811
!*
!*  REFERENCE                  : Feature 219934.OO_poly
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*    The selector is another associating entity with a vector subscript
!*    The selector is with a vector subscript again
!*    (without ssociate-name => )
!*
!*    (ICe)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE :: Base(K1,N1)    ! (4,20)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4,20)
    END TYPE

  END MODULE

  PROGRAM C811ArrVecAssoc1
  USE M
  IMPLICIT NONE

  CLASS(Base(4,:)), POINTER :: Ptr(:,:)

  ALLOCATE( Child(4,20) :: Ptr(2:10, 3:12) )

  SELECT TYPE ( As  => Ptr((/10,7,7,2/), (/12,3,3,12/)))
    TYPE IS (Base(4,*))
      STOP 20
    CLASS DEFAULT
      SELECT TYPE ( As((/1,1,1/), (/2,2,2/) ) )
        TYPE IS (Base(4,*))
          STOP 30
        CLASS DEFAULT
      END SELECT
  END SELECT
  STOP 40

  END

