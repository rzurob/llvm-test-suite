!*  ===================================================================
!*
!*  DATE                       : February 05, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Run Time Offset (RTO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : Default initialization
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!* Testing multilevel type nesting and accessing the components of the inner types
!*  - 4 levels of nesting
!*
!234567890123456789012345678901234567890123456789012345678901234567890
MODULE Mod
      IMPLICIT NONE

      TYPE Base (k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN  :: l1

        CHARACTER(l1) :: C0 = "0"
        LOGICAL        :: F0(l1) = .True.
      END TYPE

      TYPE,  EXTENDS(Base) :: Child
        CHARACTER(2*l1) :: C1 = "1"
        LOGICAL         :: F1(2*l1) = .False.
      END TYPE

      TYPE, EXTENDS(Child) :: NextGen1
        CHARACTER(4*l1) :: C2 = "2"
        LOGICAL         :: F2(4*l1) = .True.
      END TYPE

      TYPE, EXTENDS(Child) :: NextGen2
        LOGICAL         :: F2(4*l1) = .True.
        CHARACTER(4*l1) :: C2 = "2"
      END TYPE

      TYPE :: Branch
        CLASS(NextGen1(4,:)), POINTER :: cmp1
        CLASS(NextGen2(4,:)), POINTER :: cmp2
      END TYPE

      TYPE(NextGen1(4,513)), SAVE, TARGET :: tgt1
      TYPE(NextGen2(4,513)), SAVE, TARGET :: tgt2

      CONTAINS

      SUBROUTINE Select_type(Arg)
        CLASS(*) :: Arg

        SELECT TYPE ( Arg )
          CLASS IS (NextGen1(4,*))
            IF ( Arg%l1 .NE. 513 ) STOP 10

            IF ( LEN(Arg%C0) .NE.  513 ) STOP 11
            IF ( LEN(Arg%C1) .NE. 1026 ) STOP 12
            IF ( LEN(Arg%C2) .NE. 2052 ) STOP 13

            IF ( SIZE(Arg%F0) .NE.  513 ) STOP 14
            IF ( SIZE(Arg%F1) .NE. 1026 ) STOP 15
            IF ( SIZE(Arg%F2) .NE. 2052 ) STOP 16

            IF ( LBOUND(Arg%F0,1) .NE.  1 ) STOP 17
            IF ( LBOUND(Arg%F1,1) .NE.  1 ) STOP 18
            IF ( LBOUND(Arg%F2,1) .NE.  1 ) STOP 19

            IF ( UBOUND(Arg%F0,1) .NE.  513 ) STOP 20
            IF ( UBOUND(Arg%F1,1) .NE. 1026 ) STOP 21
            IF ( UBOUND(Arg%F2,1) .NE. 2052 ) STOP 22

            Arg%Child%C0 = "A"
            Arg%Child%C1 = "B"
            Arg%C2       = "C"
            Arg%Child%F0 = .True.
            Arg%Child%F1 = .True.
            Arg%F2       = .True.

          CLASS IS (NextGen2(4,*))
            IF ( Arg%l1 .NE. 513 ) STOP 23

            IF ( LEN(Arg%C0) .NE.  513 ) STOP 24
            IF ( LEN(Arg%C1) .NE. 1026 ) STOP 25
            IF ( LEN(Arg%C2) .NE. 2052 ) STOP 26

            IF ( SIZE(Arg%F0) .NE.  513 ) STOP 27
            IF ( SIZE(Arg%F1) .NE. 1026 ) STOP 28
            IF ( SIZE(Arg%F2) .NE. 2052 ) STOP 29

            IF ( LBOUND(Arg%F0,1) .NE.  1 ) STOP 30
            IF ( LBOUND(Arg%F1,1) .NE.  1 ) STOP 31
            IF ( LBOUND(Arg%F2,1) .NE.  1 ) STOP 32

            IF ( UBOUND(Arg%F0,1) .NE.  513 ) STOP 33
            IF ( UBOUND(Arg%F1,1) .NE. 1026 ) STOP 34
            IF ( UBOUND(Arg%F2,1) .NE. 2052 ) STOP 35

            Arg%Base%C0  = "E"
            Arg%Child%C1 = "F"
            Arg%C2       = "G"
            Arg%Base%F0  = .False.
            Arg%Child%F1 = .False.
            Arg%F2       = .False.

          CLASS IS (Branch)

            IF ( .NOT. ASSOCIATED(Arg%cmp1, tgt1) ) STOP 40
            ASSOCIATE ( p => Arg%cmp1 )
              IF ( p%l1 .NE. 513 ) STOP 41

              IF ( LEN(p%C0) .NE.  513 ) STOP 42
              IF ( LEN(p%C1) .NE. 1026 ) STOP 43
              IF ( LEN(p%C2) .NE. 2052 ) STOP 44

              IF ( SIZE(p%F0) .NE.  513 ) STOP 45
              IF ( SIZE(p%F1) .NE. 1026 ) STOP 46
              IF ( SIZE(p%F2) .NE. 2052 ) STOP 47

              IF ( LBOUND(p%F0,1) .NE.  1 ) STOP 48
              IF ( LBOUND(p%F1,1) .NE.  1 ) STOP 49
              IF ( LBOUND(p%F2,1) .NE.  1 ) STOP 50

              IF ( UBOUND(p%F0,1) .NE.  513 ) STOP 51
              IF ( UBOUND(p%F1,1) .NE. 1026 ) STOP 52
              IF ( UBOUND(p%F2,1) .NE. 2052 ) STOP 53

              p%C0 = "H"
              p%C1 = "I"
              p%C2 = "J"
              p%F0 = .False.
              p%F1 = .False.
              p%F2 = .False.
            END ASSOCIATE

            IF ( .NOT. ASSOCIATED(Arg%cmp2, tgt2) ) STOP 54
            ASSOCIATE ( p => Arg%cmp2 )
              IF ( p%l1 .NE. 513 ) STOP 55

              IF ( LEN(p%C0) .NE.  513 ) STOP 56
              IF ( LEN(p%C1) .NE. 1026 ) STOP 57
              IF ( LEN(p%C2) .NE. 2052 ) STOP 58

              IF ( SIZE(p%F0) .NE.  513 ) STOP 59
              IF ( SIZE(p%F1) .NE. 1026 ) STOP 60
              IF ( SIZE(p%F2) .NE. 2052 ) STOP 61

              IF ( LBOUND(p%F0,1) .NE.  1 ) STOP 62
              IF ( LBOUND(p%F1,1) .NE.  1 ) STOP 63
              IF ( LBOUND(p%F2,1) .NE.  1 ) STOP 64

              IF ( UBOUND(p%F0,1) .NE.  513 ) STOP 65
              IF ( UBOUND(p%F1,1) .NE. 1026 ) STOP 66
              IF ( UBOUND(p%F2,1) .NE. 2052 ) STOP 67

              p%C0 = "K"
              p%C1 = "L"
              p%C2 = "M"
              p%F0 = .True.
              p%F1 = .True.
              p%F2 = .True.
            END ASSOCIATE

          CLASS DEFAULT
            STOP 69
        END SELECT
      END SUBROUTINE
END MODULE

PROGRAM DTPMultipleNesting06
    USE Mod
    IMPLICIT CLASS(*)(U)
    POINTER :: U

    U => tgt1
    IF ( .NOT. ASSOCIATED(U) ) STOP 110

    CALL Select_type ( U )
    IF ( TRIM(tgt1%C0) .NE. "A" ) STOP 70
    IF ( TRIM(tgt1%C1) .NE. "B" ) STOP 71
    IF ( TRIM(tgt1%C2) .NE. "C" ) STOP 72
    IF ( ANY(tgt1%F0 .NEQV. .True.) ) STOP 73
    IF ( ANY(tgt1%F1 .NEQV. .True.) ) STOP 74
    IF ( ANY(tgt1%F2 .NEQV. .True.) ) STOP 75

    U => tgt2
    IF ( .NOT. ASSOCIATED(U) ) STOP 111

    CALL Select_type ( U )
    IF ( TRIM(tgt2%C0) .NE. "E" ) STOP 80
    IF ( TRIM(tgt2%C1) .NE. "F" ) STOP 81
    IF ( TRIM(tgt2%C2) .NE. "G" ) STOP 82
    IF ( ANY(tgt2%F0 .NEQV. .False.) ) STOP 83
    IF ( ANY(tgt2%F1 .NEQV. .False.) ) STOP 84
    IF ( ANY(tgt2%F2 .NEQV. .False.) ) STOP 85

    ALLOCATE ( U, SOURCE = Branch(tgt1,tgt2) )
    IF ( .NOT. ASSOCIATED(U) ) STOP 112

    CALL Select_type ( U )
    IF ( TRIM(tgt1%C0) .NE. "H" ) STOP 90
    IF ( TRIM(tgt1%C1) .NE. "I" ) STOP 91
    IF ( TRIM(tgt1%C2) .NE. "J" ) STOP 92
    IF ( ANY(tgt1%F0 .NEQV. .False.) ) STOP 93
    IF ( ANY(tgt1%F1 .NEQV. .False.) ) STOP 94
    IF ( ANY(tgt1%F2 .NEQV. .False.) ) STOP 95
    IF ( TRIM(tgt2%C0) .NE. "K" ) STOP 96
    IF ( TRIM(tgt2%C1) .NE. "L" ) STOP 97
    IF ( TRIM(tgt2%C2) .NE. "M" ) STOP 98
    IF ( ANY(tgt2%F0 .NEQV. .True.) ) STOP 99
    IF ( ANY(tgt2%F1 .NEQV. .True.) ) STOP 100
    IF ( ANY(tgt2%F2 .NEQV. .True.) ) STOP 101

END PROGRAM DTPMultipleNesting06
