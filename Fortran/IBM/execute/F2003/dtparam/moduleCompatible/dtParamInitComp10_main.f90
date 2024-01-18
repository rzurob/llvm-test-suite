!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 06, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Default initialization for component
!*
!*  REFERENCE                  : Feature Number 289057
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Default initialization for Components
!*
!* (340302/340309)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  PROGRAM dtParamInitComp10
  USE M

  CLASS(*), POINTER     :: Ptr
  CLASS(*), ALLOCATABLE :: Alloc

  ALLOCATE(Ptr, source=createObj())

  SELECT TYPE( As => Ptr)
  TYPE IS (DT1(4, L=*,K1=4,L1=*))

    IF ( As%K        .NE. 4 )                   STOP 11
    IF ( As%L        .NE. 4 )                   STOP 12
    IF ( As%K1       .NE. 4 )                   STOP 13
    IF ( As%L1       .NE. 4 )                   STOP 14

    IF ( As%T%K      .NE. 4 )                   STOP 15
    IF ( As%T%L      .NE. 4 )                   STOP 16
    IF ( As%T1%K     .NE. 4 )                   STOP 17
    IF ( As%T1%L     .NE. 4 )                   STOP 18

    IF ( KIND(As%T%I)   .NE. 4 )                STOP 21
    IF ( SIZE(As%T%I)   .NE. 4 )                STOP 22
    IF ( ANY(As%T%I     .NE. 4))                STOP 23

    IF ( KIND(As%T%R)   .NE. 4 )                STOP 31
    IF ( SIZE(As%T%R)   .NE. 4 )                STOP 32
    IF ( ANY(As%T%R     .NE. 4))                STOP 33

  CLASS DEFAULT
    STOP 55
  END SELECT


  ALLOCATE(Alloc, source=createObj48())

  SELECT TYPE( As => Alloc)
  TYPE IS (DT1(4,*,K1=8,L1=*))

    IF ( As%K        .NE. 4 )                   STOP 41
    IF ( As%L        .NE. 4 )                   STOP 42
    IF ( As%K1       .NE. 8 )                   STOP 43
    IF ( As%L1       .NE. 8 )                   STOP 44

    IF ( As%T%K      .NE. 4 )                   STOP 45
    IF ( As%T%L      .NE. 4 )                   STOP 46
    IF ( As%T1%K     .NE. 8 )                   STOP 47
    IF ( As%T1%L     .NE. 8 )                   STOP 48

    IF ( KIND(As%T%I)   .NE. 4 )                STOP 51
    IF ( SIZE(As%T%I)   .NE. 4 )                STOP 52
    IF ( ANY(As%T%I     .NE. 4))                STOP 53

    IF ( KIND(As%T%R)   .NE. 4 )                STOP 61
    IF ( SIZE(As%T%R)   .NE. 4 )                STOP 62
    IF ( ANY(As%T%R     .NE. 4))                STOP 63

    IF ( KIND(As%T1%I)   .NE. 8 )                STOP 51
    IF ( SIZE(As%T1%I)   .NE. 8 )                STOP 52
    IF ( ANY(As%T1%I     .NE. 8))                STOP 53

    IF ( KIND(As%T1%R)   .NE. 8 )                STOP 61
    IF ( SIZE(As%T1%R)   .NE. 8 )                STOP 62
    IF ( ANY(As%T1%R     .NE. 8))                STOP 63

  CLASS DEFAULT
    STOP 77
  END SELECT


  END

