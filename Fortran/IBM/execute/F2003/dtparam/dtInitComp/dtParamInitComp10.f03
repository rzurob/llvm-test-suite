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


  MODULE M

    TYPE :: DT(K, L)
      INTEGER(8), KIND :: K
      INTEGER(8), LEN  :: L
      INTEGER(K)    :: I(L)
      REAL(K)       :: R(L)
    END TYPE

    TYPE, EXTENDS(DT) :: DT1(K1,L1)
      INTEGER(1), KIND :: K1
      INTEGER(1), LEN  :: L1
      TYPE(DT(K1,:)), POINTER :: T2
      TYPE(DT(K1,:)), ALLOCATABLE :: T3

      TYPE(DT(4,4)) :: T
      TYPE(DT(K1,L1)) :: T1
    END TYPE

    contains

    function createObj ()
        type(DT1(4,4,4,4)) createObj

        createObj%i = 4
        createObj%r = 4.0
        nullify (createObj%t2)
        createObj%t%i = 4
        createObj%t%r = 4.0
        createObj%t1%i = 4
        createObj%t1%r = 4.0
    end function


    function createObj48 ()
        type(DT1(4,4,8,8)) createObj48

        createObj48%i = 4
        createObj48%r = 4.0
        nullify (createObj48%t2)
        createObj48%t%i = 4
        createObj48%t%r = 4.0
        createObj48%t1%i = 8
        createObj48%t1%r = 8.0
    end function

  END MODULE


  PROGRAM dtParamInitComp10
  USE M

  CLASS(*), POINTER     :: Ptr
  CLASS(*), ALLOCATABLE :: Alloc

  ALLOCATE(Ptr, source=createObj())

  SELECT TYPE( As => Ptr)
  TYPE IS (DT1(4, L=*,K1=4,L1=*))

    IF ( As%K        .NE. 4 )                   ERROR STOP 11
    IF ( As%L        .NE. 4 )                   ERROR STOP 12
    IF ( As%K1       .NE. 4 )                   ERROR STOP 13
    IF ( As%L1       .NE. 4 )                   ERROR STOP 14

    IF ( As%T%K      .NE. 4 )                   ERROR STOP 15
    IF ( As%T%L      .NE. 4 )                   ERROR STOP 16
    IF ( As%T1%K     .NE. 4 )                   ERROR STOP 17
    IF ( As%T1%L     .NE. 4 )                   ERROR STOP 18

    IF ( KIND(As%T%I)   .NE. 4 )                ERROR STOP 21
    IF ( SIZE(As%T%I)   .NE. 4 )                ERROR STOP 22
    IF ( ANY(As%T%I     .NE. 4))                ERROR STOP 23

    IF ( KIND(As%T%R)   .NE. 4 )                ERROR STOP 31
    IF ( SIZE(As%T%R)   .NE. 4 )                ERROR STOP 32
    IF ( ANY(As%T%R     .NE. 4))                ERROR STOP 33

  CLASS DEFAULT
    STOP 55
  END SELECT


  ALLOCATE(Alloc, source=createObj48())

  SELECT TYPE( As => Alloc)
  TYPE IS (DT1(4,*,K1=8,L1=*))

    IF ( As%K        .NE. 4 )                   ERROR STOP 41
    IF ( As%L        .NE. 4 )                   ERROR STOP 42
    IF ( As%K1       .NE. 8 )                   ERROR STOP 43
    IF ( As%L1       .NE. 8 )                   ERROR STOP 44

    IF ( As%T%K      .NE. 4 )                   ERROR STOP 45
    IF ( As%T%L      .NE. 4 )                   ERROR STOP 46
    IF ( As%T1%K     .NE. 8 )                   ERROR STOP 47
    IF ( As%T1%L     .NE. 8 )                   ERROR STOP 48

    IF ( KIND(As%T%I)   .NE. 4 )                ERROR STOP 51
    IF ( SIZE(As%T%I)   .NE. 4 )                ERROR STOP 52
    IF ( ANY(As%T%I     .NE. 4))                ERROR STOP 53

    IF ( KIND(As%T%R)   .NE. 4 )                ERROR STOP 61
    IF ( SIZE(As%T%R)   .NE. 4 )                ERROR STOP 62
    IF ( ANY(As%T%R     .NE. 4))                ERROR STOP 63

    IF ( KIND(As%T1%I)   .NE. 8 )                ERROR STOP 51
    IF ( SIZE(As%T1%I)   .NE. 8 )                ERROR STOP 52
    IF ( ANY(As%T1%I     .NE. 8))                ERROR STOP 53

    IF ( KIND(As%T1%R)   .NE. 8 )                ERROR STOP 61
    IF ( SIZE(As%T1%R)   .NE. 8 )                ERROR STOP 62
    IF ( ANY(As%T1%R     .NE. 8))                ERROR STOP 63

  CLASS DEFAULT
    STOP 77
  END SELECT


  END
