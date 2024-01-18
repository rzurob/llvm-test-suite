! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Exec/ArrSection1.f
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  ArrSection1.f  
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
!*  TEST CASE NAME             : ArrSection1
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Nov. 02, 2004
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature 219934
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*    The selector is an array  formed from component of array section
!*    (ICE) 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE Zero(K1,N1)    ! (4,20)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
    END TYPE

    TYPE, EXTENDS(Zero) :: Base    ! (4,20)
      INTEGER(K1) :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4,20)
      INTEGER(K1)  :: ChildId = 2
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId 
    END TYPE

    CONTAINS

    ELEMENTAL FUNCTION GetChildId(Arg)
    CLASS(Child(4,*)), INTENT(IN) :: Arg
    INTEGER      :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    ELEMENTAL FUNCTION GetBaseId(Arg)
    CLASS(Base(4,*)), INTENT(IN) :: Arg
    INTEGER      :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

  END MODULE

  PROGRAM ArrSection1
  USE M
  IMPLICIT NONE
  INTEGER :: i

  TYPE(Child(4,20)) :: Arr(4, 4)
 
  Arr = RESHAPE((/(Child(4,20)(BaseId=i, ChildId=-i), i=1, 16) /), (/4,4/)) 

  ASSOCIATE ( As => Arr(1:4:2, 2:4:2)%Base)
    IF ( ANY (LBOUND(As)      .NE. (/1,1/) ) )             STOP 30
    IF ( ANY (SHAPE(As)       .NE. (/2,2/) ) )             STOP 32
    IF ( ANY (As%GetID() .NE. RESHAPE((/ 5, 7, 13, 15/), (/2,2/)) ) ) STOP 34 
  END ASSOCIATE

  ASSOCIATE ( As => Arr(1:4:2, 2:4:2)%ChildId)
    IF ( ANY (LBOUND(As)   .NE. (/1,1/) ) )             STOP 40
    IF ( ANY (SHAPE(As)    .NE. (/2,2/) ) )             STOP 42
    IF ( ANY (As           .NE. RESHAPE((/ -5, -7, -13, -15/), (/2,2/)) ) ) STOP 44 
  END ASSOCIATE

  ASSOCIATE ( As => Arr(1:4:2, 2:4:2)%Base%GetId())
    IF ( ANY (LBOUND(As)   .NE. (/1,1/) ) )             STOP 30
    IF ( ANY (SHAPE(As)    .NE. (/2,2/) ) )             STOP 32
    IF ( ANY (As           .NE. RESHAPE((/ 5, 7, 13, 15/), (/2,2/)) ) ) STOP 34 
  END ASSOCIATE


  END
