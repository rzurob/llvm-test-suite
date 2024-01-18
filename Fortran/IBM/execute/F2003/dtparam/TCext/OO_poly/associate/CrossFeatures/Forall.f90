! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/CrossFeatures/Forall.f
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  Forall.f
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
!*  TEST CASE NAME             : Forall 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Mar. 10, 2005
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
!*    The forall stmt
!*    () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

 
  MODULE M
    TYPE :: DT(K1)    ! (4)
      INTEGER, KIND :: K1
      INTEGER(K1)   :: Id = 1
      CONTAINS
      PROCEDURE, PASS   :: GetId
    END TYPE

  CONTAINS

    ELEMENTAL FUNCTION GetId(Arg)
    IMPLICIT CLASS(DT(4))(A) 
    INTENT(IN) :: Arg
    INTEGER    :: GetId
      GetId = Arg%Id
    END FUNCTION

  END MODULE
 
  PROGRAM Forall 

  USE M
  IMPLICIT TYPE(DT(4))(A) 
  DIMENSION :: Arr(2:130)
  LOGICAL(8) :: LArr(2:130)
  INTEGER :: i

  ASSOCIATE ( As => (/(DT(4)(-3), i=1, 131)/) )
    FORALL (i=2:130:2)
      Arr(i)  = As(i)
    END FORALL 
  END ASSOCIATE

  IF ( Any(Arr(2::2)%ID      .NE. -3) ) STOP 60
  IF ( ANY(Arr(2::2)%GetID() .NE. -3) ) STOP 61

  ASSOCIATE ( As => Arr )
    FORALL (i=2:130:1)
      As%ID = -2 
    END FORALL 
  END ASSOCIATE

  IF ( Any(Arr%ID      .NE. -2) ) STOP 70
  IF ( ANY(Arr%GetID() .NE. -2) ) STOP 71

  ASSOCIATE ( As => Arr )
    FORALL (i=2:130, As(i)%ID .EQ. -2)
      As(i)%ID = -3 
    END FORALL 
  END ASSOCIATE

  IF ( Any(Arr%ID      .NE. -3) ) STOP 80
  IF ( ANY(Arr%GetID() .NE. -3) ) STOP 81

  ASSOCIATE ( As => Arr(:) )
    FORALL ( i=1:129 )
      As(i)%ID = As(i)%ID + 5 
    END FORALL 
  END ASSOCIATE

  IF ( Any(Arr%ID      .NE. 2) ) STOP 90
  IF ( ANY(Arr%GetID() .NE. 2) ) STOP 91

  END


