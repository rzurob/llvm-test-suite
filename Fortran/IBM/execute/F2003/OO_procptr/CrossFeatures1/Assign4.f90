! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 16, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 289058
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  A derived-type intrinsic assignment
!*  Array
!*  (306485)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    INTERFACE
      FUNCTION CToC(Arg)
        CHARACTER(*) :: Arg(:)
        CHARACTER(LEN(Arg)) :: CToc(LEN(Arg))
      END FUNCTION
    END INTERFACE

    TYPE :: Base
      PROCEDURE(CToC), NOPASS, POINTER :: ProcPtr=>NULL()
    END TYPE

    TYPE :: DT
      INTEGER :: Id
      TYPE(Base), ALLOCATABLE :: BComp(:)
    END TYPE


    CONTAINS

    FUNCTION Fun(Arg)
    CHARACTER(*) :: Arg(:)
    CHARACTER(LEN(Arg)) :: Fun(SIZE(Arg))
      Fun = Arg
    END FUNCTION


  END MODULE


  PROGRAM Assign4
  USE M
  IMPLICIT NONE

  TYPE (DT)                :: V(1)
  CHARACTER(1025)          :: Str="1"
  CHARACTER(1025)          :: U(2)

  ALLOCATE(V(1)%BComp(2))
  V = DT(-1, (/Base(RetPtr(Fun)),Base(RetPtr(Fun))/))

  IF ( ANY(V%Id .NE. -1) ) STOP 11
  IF ( .NOT. ALLOCATED(V(1)%BComp) )  STOP 12
  IF ( .NOT. ASSOCIATED(V(1)%BComp(1)%ProcPtr, RetPtr(Fun)) ) STOP 13
  IF ( .NOT. ASSOCIATED(V(1)%BComp(2)%ProcPtr, RetPtr(Fun)) ) STOP 10

  IF (ANY(V(1)%BComp(1)%ProcPtr((/"ABC", "123"/)) .NE. (/"ABC", "123"/)) ) STOP 14
  IF (ANY(V(1)%BComp(2)%ProcPtr((/"", ""/)) .NE. (/"", ""/)) )             STOP 15
  IF (ANY(V(1)%BComp(1)%ProcPtr((/Str, Str/)) .NE. (/Str, Str/)) )         STOP 16
  IF (ANY(V(1)%BComp(2)%ProcPtr((/Str, Str/)) .NE. (/Str, Str/)) )         STOP 17

  U= V(1)%BComp(1)%ProcPtr((/"1", "1" /)) ! iced here
  IF (ANY(U .NE. (/"1", "1"/)) ) STOP 24

  CONTAINS

  FUNCTION RetPtr(Arg)
  PROCEDURE(CToC), POINTER :: RetPtr
  PROCEDURE(CToC) :: Arg
    RetPtr => Arg
  END FUNCTION

  END

