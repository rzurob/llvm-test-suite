! GB DTP extension using:
! ftcx_dtp -qnol -qnodefaultpv -qreuse=self -qreuse=base /tstdev/OO_poly/selectType/Quotes/SltTypeComp.f
! opt variations: -ql -qdefaultpv -qreuse=none

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
!*   The type spec is specified with a type with variuos components
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base(K1)    ! (4)
      INTEGER, KIND           :: K1
      INTEGER(K1)             :: BaseId=-1
      TYPE(Base(K1)), POINTER :: BasePtr(:)=>NULL()
    END TYPE

    TYPE, ABSTRACT,  EXTENDS(Base) :: Base1    ! (4)
      INTEGER(K1) :: Base1Id=1
      TYPE(BASE(K1)), ALLOCATABLE :: BaseArr(:)
      CLASS(Base1(K1)), POINTER :: Base1Ptr => NULL()
    END TYPE

    TYPE, EXTENDS(Base1) :: Child    ! (4)
      CLASS(*), ALLOCATABLE :: UVar
      CLASS(Base1(K1)), POINTER  :: Base2Ptr
    END TYPE

  END MODULE

  PROGRAM SltTypeComp
  USE M
  IMPLICIT NONE

  CLASS(*), POINTER :: Var
  TYPE(Child(4)), TARGET :: Tar

  Var => Tar

  SELECT TYPE (As => Var)
   CLASS DEFAULT
     STOP 19
   CLASS IS (Base(4))
     STOP 20
   TYPE IS (Base(4))
     STOP 21
   CLASS IS (Child(4))
     STOP 22
   TYPE IS (Child(4))

     IF (As%BaseId             .NE. -1  )  STOP 31
     ALLOCATE(As%BasePtr(3))
     IF (ANY(As%BasePtr%BaseId .NE. -1) )  STOP 31

     ALLOCATE(As%Base1Ptr, SOURCE=Child(4)(BaseArr=NULL(), UVar=NULL(), Base2Ptr=NULL()))
     SELECT TYPE(As1 => As%Base1Ptr)
       TYPE IS (Child(4))
         IF ( As1%BaseId .NE. -1 ) STOP
         IF ( As1%Base1Id .NE. 1 ) STOP
       CLASS DEFAULT
         STOP
     END SELECT

     ALLOCATE(Child(4) :: As%Base2Ptr)
     IF ( As%Base2Ptr%BaseId .NE. -1 ) STOP
     IF ( As%Base2Ptr%Base1Id .NE. 1 ) STOP

  END SELECT

  END
