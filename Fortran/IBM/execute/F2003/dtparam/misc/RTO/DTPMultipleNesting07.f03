!*  ===================================================================
!*
!*  DATE                       : February 05, 2008
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
        CLASS(Base(4,:)), POINTER :: cmp1
        CLASS(Base(4,:)), POINTER :: cmp2
      END TYPE

      TYPE(NextGen1(4,513)), SAVE, TARGET :: tgt1
      TYPE(Child(4,513)), SAVE, TARGET :: tgt2

      CONTAINS

      SUBROUTINE Select_type(Arg)
        CLASS(*) :: Arg

        SELECT TYPE ( Arg )
          CLASS IS (NextGen1(4,*))
            IF ( Arg%l1 .NE. 513 ) ERROR STOP 10

            IF ( LEN(Arg%C0) .NE.  513 ) ERROR STOP 11
            IF ( LEN(Arg%C1) .NE. 1026 ) ERROR STOP 12
            IF ( LEN(Arg%C2) .NE. 2052 ) ERROR STOP 13

            IF ( SIZE(Arg%F0) .NE.  513 ) ERROR STOP 14
            IF ( SIZE(Arg%F1) .NE. 1026 ) ERROR STOP 15
            IF ( SIZE(Arg%F2) .NE. 2052 ) ERROR STOP 16

            IF ( LBOUND(Arg%F0,1) .NE.  1 ) ERROR STOP 17
            IF ( LBOUND(Arg%F1,1) .NE.  1 ) ERROR STOP 18
            IF ( LBOUND(Arg%F2,1) .NE.  1 ) ERROR STOP 19

            IF ( UBOUND(Arg%F0,1) .NE.  513 ) ERROR STOP 20
            IF ( UBOUND(Arg%F1,1) .NE. 1026 ) ERROR STOP 21
            IF ( UBOUND(Arg%F2,1) .NE. 2052 ) ERROR STOP 22

            Arg%Child%C0 = "A"
            Arg%Child%C1 = "B"
            Arg%C2       = "C"
            Arg%Child%F0 = .True.
            Arg%Child%F1 = .True.
            Arg%F2       = .True.

          CLASS IS (Child(4,*))
            IF ( Arg%l1 .NE. 513 ) ERROR STOP 23

            IF ( LEN(Arg%C0) .NE.  513 ) ERROR STOP 24
            IF ( LEN(Arg%C1) .NE. 1026 ) ERROR STOP 25

            IF ( SIZE(Arg%F0) .NE.  513 ) ERROR STOP 26
            IF ( SIZE(Arg%F1) .NE. 1026 ) ERROR STOP 27

            IF ( LBOUND(Arg%F0,1) .NE.  1 ) ERROR STOP 28
            IF ( LBOUND(Arg%F1,1) .NE.  1 ) ERROR STOP 29

            IF ( UBOUND(Arg%F0,1) .NE.  513 ) ERROR STOP 30
            IF ( UBOUND(Arg%F1,1) .NE. 1026 ) ERROR STOP 31

            Arg%Base%C0  = "E"
            Arg%C1 = "F"
            Arg%Base%F0  = .False.
            Arg%F1 = .False.

          CLASS IS (Branch)

            IF ( .NOT. ASSOCIATED(Arg%cmp1, tgt1) ) ERROR STOP 40
            ASSOCIATE ( p => Arg%cmp1 )
              IF ( p%l1 .NE. 513 ) ERROR STOP 41
              SELECT TYPE ( p )
                 CLASS IS (NextGen1(4,*))
                    IF ( LEN(p%C0) .NE.  513 ) ERROR STOP 42
                    IF ( LEN(p%C1) .NE. 1026 ) ERROR STOP 43
                    IF ( LEN(p%C2) .NE. 2052 ) ERROR STOP 44

                    IF ( SIZE(p%F0) .NE.  513 ) ERROR STOP 45
                    IF ( SIZE(p%F1) .NE. 1026 ) ERROR STOP 46
                    IF ( SIZE(p%F2) .NE. 2052 ) ERROR STOP 47

                    IF ( LBOUND(p%F0,1) .NE.  1 ) ERROR STOP 48
                    IF ( LBOUND(p%F1,1) .NE.  1 ) ERROR STOP 49
                    IF ( LBOUND(p%F2,1) .NE.  1 ) ERROR STOP 50

                    IF ( UBOUND(p%F0,1) .NE.  513 ) ERROR STOP 51
                    IF ( UBOUND(p%F1,1) .NE. 1026 ) ERROR STOP 52
                    IF ( UBOUND(p%F2,1) .NE. 2052 ) ERROR STOP 53

                    p%C0 = "H"
                    p%C1 = "I"
                    p%C2 = "J"
                    p%F0 = .False.
                    p%F1 = .False.
                    p%F2 = .False.

                 CLASS DEFAULT
                   STOP 120
              END SELECT
            END ASSOCIATE

            IF ( .NOT. ASSOCIATED(Arg%cmp2, tgt2) ) ERROR STOP 54
            ASSOCIATE ( p => Arg%cmp2 )
              IF ( p%l1 .NE. 513 ) ERROR STOP 55
              SELECT TYPE ( p )
                 CLASS IS (Child(4,*))
                    IF ( LEN(p%C0) .NE.  513 ) ERROR STOP 56
                    IF ( LEN(p%C1) .NE. 1026 ) ERROR STOP 57

                    IF ( SIZE(p%F0) .NE.  513 ) ERROR STOP 58
                    IF ( SIZE(p%F1) .NE. 1026 ) ERROR STOP 59

                    IF ( LBOUND(p%F0,1) .NE.  1 ) ERROR STOP 60
                    IF ( LBOUND(p%F1,1) .NE.  1 ) ERROR STOP 61

                    IF ( UBOUND(p%F0,1) .NE.  513 ) ERROR STOP 62
                    IF ( UBOUND(p%F1,1) .NE. 1026 ) ERROR STOP 63

                    p%C0 = "K"
                    p%C1 = "L"
                    p%F0 = .True.
                    p%F1 = .True.

                 CLASS DEFAULT
                   STOP 121
              END SELECT
            END ASSOCIATE

          CLASS DEFAULT
            STOP 122
        END SELECT
      END SUBROUTINE
END MODULE

PROGRAM DTPMultipleNesting07
    USE Mod
    IMPLICIT CLASS(*)(U)
    POINTER :: U

    U => tgt1
    IF ( .NOT. ASSOCIATED(U) ) ERROR STOP 110
    CALL Select_type ( U )
    IF ( TRIM(tgt1%C0) .NE. "A" ) ERROR STOP 70
    IF ( TRIM(tgt1%C1) .NE. "B" ) ERROR STOP 71
    IF ( TRIM(tgt1%C2) .NE. "C" ) ERROR STOP 72
    IF ( ANY(tgt1%F0 .NEQV. .True.) ) ERROR STOP 73
    IF ( ANY(tgt1%F1 .NEQV. .True.) ) ERROR STOP 74
    IF ( ANY(tgt1%F2 .NEQV. .True.) ) ERROR STOP 75

    U => tgt2
    IF ( .NOT. ASSOCIATED(U) ) ERROR STOP 111
    CALL Select_type ( U )
    IF ( TRIM(tgt2%C0) .NE. "E" ) ERROR STOP 80
    IF ( TRIM(tgt2%C1) .NE. "F" ) ERROR STOP 81
    IF ( ANY(tgt2%F0 .NEQV. .False.) ) ERROR STOP 82
    IF ( ANY(tgt2%F1 .NEQV. .False.) ) ERROR STOP 83

    ALLOCATE ( U, SOURCE = Branch(tgt1,tgt2) )
    IF ( .NOT. ASSOCIATED(U) ) ERROR STOP 112
    CALL Select_type ( U )
    IF ( TRIM(tgt1%C0) .NE. "H" ) ERROR STOP 90
    IF ( TRIM(tgt1%C1) .NE. "I" ) ERROR STOP 91
    IF ( TRIM(tgt1%C2) .NE. "J" ) ERROR STOP 92
    IF ( ANY(tgt1%F0 .NEQV. .False.) ) ERROR STOP 93
    IF ( ANY(tgt1%F1 .NEQV. .False.) ) ERROR STOP 94
    IF ( ANY(tgt1%F2 .NEQV. .False.) ) ERROR STOP 95
    IF ( TRIM(tgt2%C0) .NE. "K" ) ERROR STOP 96
    IF ( TRIM(tgt2%C1) .NE. "L" ) ERROR STOP 97
    IF ( ANY(tgt2%F0 .NEQV. .True.) ) ERROR STOP 98
    IF ( ANY(tgt2%F1 .NEQV. .True.) ) ERROR STOP 99

END PROGRAM DTPMultipleNesting07
