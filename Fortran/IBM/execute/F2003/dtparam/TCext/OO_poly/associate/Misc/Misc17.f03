! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/Misc/Misc17.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 04, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 219934
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!* "1516-078 (S) Operands must be conformable" and incorrect output
!*  for "print*, As%BaseArr(1,1)%BaseId"
!* (297811 )
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE Zero(K1,N1)    ! (4,20)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
    END TYPE

    TYPE, EXTENDS(Zero) :: Base    ! (4,20)
      INTEGER(K1) :: BaseId = 1
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4,20)
      INTEGER(K1)  :: ChildId = 2
      TYPE(Base(K1,N1)) :: BaseArr(1,1)
    END TYPE

  END MODULE

  PROGRAM Misc17
  USE M
  IMPLICIT NONE
  CLASS(Base(4,:)), ALLOCATABLE :: Var
  integer i

  ALLOCATE(Child(4,20) :: Var)

  SELECT TYPE ( As  => RESHAPE( (/(Var,  i=1,4)/), (/2,2/)) )
  TYPE IS (Child(4,*))

    print*, SHAPE(As)
    print*, SHAPE(As%BaseArr(1,1)%BaseId)
    print*, As%BaseArr(1,1)%BaseId
    IF ( ANY (SHAPE(As%BaseArr(1,1)%BaseId) .NE. (/2,2/) ) )  ERROR STOP 35
    IF ( ANY(As%BaseArr(1,1)%BaseId  .NE. 1) ) ERROR STOP 36

  END SELECT

  END

