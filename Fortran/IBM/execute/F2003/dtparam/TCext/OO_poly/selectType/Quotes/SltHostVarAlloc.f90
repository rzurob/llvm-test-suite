! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=none /tstdev/OO_poly/selectType/Quotes/SltHostVarAlloc.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=self -qreuse=base

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: SltHostVarAlloc.f
! %VERIFY:  
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: 
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : SltHostVarAlloc
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Dec. 23, 2004
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type 
!*
!*  SECONDARY FUNCTIONS TESTED : Selector 
!*
!*  REFERENCE                  : Feature 219934.OO_poly
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*     
!*   The selector is an associate name associating to an allocatable 
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE  :: Zero(K1,N1)    ! (4,20)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
    END TYPE 

    TYPE, EXTENDS(Zero)  :: Base(N2,K2)    ! (4,20,20,4)
      INTEGER, KIND :: K2
      INTEGER, LEN  :: N2
      INTEGER(K2)   :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
      PROCEDURE, PASS   :: SetId => SetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child(N3,K3)    ! (4,20,20,4,20,4)
      INTEGER, KIND                   :: K3
      INTEGER, LEN                    :: N3
      INTEGER(K3)                     :: ChildId = 2
      CLASS(Base(K3,:,:,K3)), POINTER :: BasePtr => NULL()
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
      PROCEDURE, PASS   :: SetId => SetChildId
    END TYPE

    CONTAINS

    ELEMENTAL FUNCTION GetChildId(Arg)
    CLASS(Child(4,*,*,4,*,4)), INTENT(IN) :: Arg
    INTEGER                  :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    ELEMENTAL FUNCTION GetBaseId(Arg)
    CLASS(Base(4,*,*,4)), INTENT(IN)  :: Arg
    INTEGER                  :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

    SUBROUTINE SetBaseId(Arg)
    CLASS(Base(4,*,*,4))  :: Arg
      Arg%BaseId = -1
    END SUBROUTINE

    SUBROUTINE SetChildId(Arg)
    CLASS(Child(4,*,*,4,*,4))  :: Arg
      Arg%ChildId = -2
    END SUBROUTINE

  END MODULE


  PROGRAM SltHostVarAlloc
  USE M
  IMPLICIT  NONE
  CLASS(Zero(4,:)), ALLOCATABLE :: Var

  ALLOCATE( Child(4,20,20,4,20,4) :: Var)
  SELECT TYPE ( As => Var )
    CLASS DEFAULT
      SELECT TYPE ( Var )
        CLASS IS (Child(4,*,*,4,*,4)) 
          SELECT TYPE ( As )
            CLASS IS (Child(4,*,*,4,*,4)) 
              SELECT TYPE ( Var )
                CLASS DEFAULT
                  SELECT TYPE ( As )
                    CLASS DEFAULT
                      SELECT TYPE ( Var )
                        CLASS DEFAULT 
                          IF ( As%Base%GetId() .NE.  1 ) STOP 34
                          IF ( As%GetId()      .NE.  2 ) STOP 35
                          IF ( As%BaseId       .NE.  1 ) STOP 36
                          IF ( As%ChildId      .NE.  2 ) STOP 37
                          CALL As%SetId()
                          CALL As%Base%SetId()
                          IF ( As%Base%GetId() .NE. -1 ) STOP 34
                          IF ( As%GetId()      .NE. -2 ) STOP 35
                          IF ( As%BaseId       .NE. -1 ) STOP 36
                          IF ( As%ChildId      .NE. -2 ) STOP 37
                      END SELECT
                  END SELECT
              END SELECT
          END SELECT
      END SELECT

    TYPE is (Base(4,*,*,4))
      STOP 32
    TYPE IS (Zero(4,*))
      STOP 38

  END SELECT


  END
  
