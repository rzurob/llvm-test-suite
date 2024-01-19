! GB DTP extension using:
! ftcx_dtp -ql -qnodefaultpv -qdeferredlp -qreuse=self -qreuse=base /tstdev/OO_poly/selectType/Quotes/SltTypeCompArr.f
! opt variations: -qnol -qdefaultpv -qnodeferredlp -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 14, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Selector
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
!*
!*   The type spec is specified with a type with variuos Array components
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base(N1,K1)    ! (20,4)
      INTEGER, KIND             :: K1
      INTEGER, LEN              :: N1
      INTEGER(K1)               :: BaseId=-1
      TYPE(Base(:,K1)), POINTER :: BaseArr(:)=>NULL()
    END TYPE

    TYPE, ABSTRACT,  EXTENDS(Base) :: Base1    ! (20,4)
      INTEGER(K1) :: Base1Id=1
      TYPE(BASE(:,K1)), POINTER :: BaseArr1(:)
    END TYPE

    TYPE, EXTENDS(Base1) :: Child    ! (20,4)
      INTEGER(K1) :: childId=2
      CLASS(*), POINTER  :: UArr(:)
    END TYPE

  END MODULE

  PROGRAM SltTypeCompArr
  USE M
  IMPLICIT NONE

  CLASS(*), POINTER :: Ptr(:)
  TYPE(Child(20,4)), TARGET :: Tar(3)

  Ptr => Tar
  SELECT TYPE (Ptr)
    TYPE IS (Child(*,4))
      Ptr(1)%UArr           => Ptr
      Ptr(1)%Base1%BaseArr1 => Ptr%Base
      Ptr(1)%BaseArr        => Ptr%Base
  END SELECT

  SELECT TYPE ( As => Ptr)
   CLASS DEFAULT
     STOP 19
   CLASS IS (Base(*,4))
     STOP 20
   TYPE IS (Base(*,4))
     STOP 21
   CLASS IS (Child(*,4))
     STOP 22
   TYPE IS (Child(*,4))

     IF ( ANY(As%BaseId  .NE. -1  ))  ERROR STOP 31
     IF ( ANY(As%Base1Id .NE.  1  ))  ERROR STOP 32
     IF ( ANY(As%ChildId .NE.  2  ))  ERROR STOP 33

     IF (ANY(As(1)%Base1%BaseArr1%BaseId .NE. -1) )  ERROR STOP 41
     IF (ANY(As(1)%Base%BaseArr%BaseId   .NE. -1) )  ERROR STOP 42

     SELECT TYPE(As1 => As(1)%UArr)
       TYPE IS (Child(*,4))
         IF ( ANY(As1%BaseId .NE. -1) ) ERROR STOP 51
         IF ( ANY(As1%Base1Id .NE. 1) ) ERROR STOP 52
         IF ( ANY(As1%ChildId .NE. 2) ) ERROR STOP 53
       CLASS DEFAULT
         STOP 54
     END SELECT

  END SELECT

  END
