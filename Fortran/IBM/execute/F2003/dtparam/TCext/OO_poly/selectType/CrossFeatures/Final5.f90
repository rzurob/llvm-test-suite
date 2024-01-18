! GB DTP extension using:
! ftcx_dtp -qck /tstdev/OO_poly/selectType/CrossFeatures/Final5.f
! opt variations: -qnock

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: Final5.f 
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
!*  TEST CASE NAME             : Final5 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Feb. 02, 2005
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
!* Finalization
!* (299542)
!* 
!*
!234567890123456789012345678901234567890123456789012345678901234567890





  MODULE M
    TYPE  :: DT(K1,N1)    ! (1,513)
      INTEGER, KIND             :: K1
      INTEGER, LEN              :: N1
      CHARACTER(kind=K1,len=N1) :: C0="0"
      CONTAINS
      Final :: FinalDT
    END TYPE

    LOGICAL :: Final(10) = .FALSE.
    INTEGER :: Index = 0

    CONTAINS

    SUBROUTINE FinalDT(Arg)
    TYPE(DT(1,*)) :: Arg
      Index = Index + 1
      Final(Index) = .TRUE.
    END SUBROUTINE
    
  END MODULE

  PROGRAM final5
  USE M
  IMPLICIT CLASS(*)(U)
  Type(DT(1,513)), PARAMETER :: U=DT(1,513)(C0="-0")
  TYPE(DT(1,513)) :: V,W

  print*, Final
  print*, V%C0 

  Final = .FALSE.
  Index = 0
  V = U 

  IF (ANY(Final(1:1) .NEQV. .TRUE.))  STOP 21
  IF (ANY(Final(3: ) .NEQV. .FALSE.)) STOP 23

  Final = .FALSE.
  Index = 0
  CALL Sub(V)

  IF (ANY(Final(1:2) .NEQV. .TRUE. )) STOP 33
  IF (ANY(Final(3: ) .NEQV. .FALSE.)) STOP 34

  CONTAINS

  SUBROUTINE  Sub(UArg)

  SELECT TYPE ( UArg )
  TYPE IS (DT(1,*))
    IF (ANY(Final(:) .NEQV. .FALSE.)) STOP 41
    UArg = DT(1,513)() 
    IF (ANY(Final(1:2) .NEQV. .TRUE. )) STOP 42
    IF (ANY(Final(3: ) .NEQV. .FALSE.)) STOP 43
  CLASS DEFAULT
    STOP 61
  END SELECT

  !No new finalization
  IF (ANY(Final(1:2) .NEQV. .TRUE.))  STOP 53
  IF (ANY(Final(3: ) .NEQV. .FALSE.)) STOP 55

  END SUBROUTINE

  END


