!*  ===================================================================
!*
!*  DATE                       : February 15, 2008
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
        CLASS(*), POINTER :: cmp1
        CLASS(*), POINTER :: cmp2
      END TYPE

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

          CLASS IS (NextGen2(4,*))
            IF ( Arg%l1 .NE. 513 ) ERROR STOP 23

            IF ( LEN(Arg%C0) .NE.  513 ) ERROR STOP 24
            IF ( LEN(Arg%C1) .NE. 1026 ) ERROR STOP 25
            IF ( LEN(Arg%C2) .NE. 2052 ) ERROR STOP 26

            IF ( SIZE(Arg%F0) .NE.  513 ) ERROR STOP 27
            IF ( SIZE(Arg%F1) .NE. 1026 ) ERROR STOP 28
            IF ( SIZE(Arg%F2) .NE. 2052 ) ERROR STOP 29

            IF ( LBOUND(Arg%F0,1) .NE.  1 ) ERROR STOP 30
            IF ( LBOUND(Arg%F1,1) .NE.  1 ) ERROR STOP 31
            IF ( LBOUND(Arg%F2,1) .NE.  1 ) ERROR STOP 32

            IF ( UBOUND(Arg%F0,1) .NE.  513 ) ERROR STOP 33
            IF ( UBOUND(Arg%F1,1) .NE. 1026 ) ERROR STOP 34
            IF ( UBOUND(Arg%F2,1) .NE. 2052 ) ERROR STOP 35

            Arg%Base%C0  = "E"
            Arg%Child%C1 = "F"
            Arg%C2       = "G"
            Arg%Base%F0  = .False.
            Arg%Child%F1 = .False.
            Arg%F2       = .False.

          CLASS IS (Branch)

            ASSOCIATE ( p => Arg%cmp1 )
              SELECT TYPE ( p )
                 CLASS IS (NextGen1(4,*))
                   IF ( p%l1 .NE. 513 ) ERROR STOP 41

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
                   STOP 121
              END SELECT
            END ASSOCIATE

            ASSOCIATE ( p => Arg%cmp2 )
              SELECT TYPE ( p )
                 CLASS IS (NextGen2(4,*))
                   IF ( p%l1 .NE. 513 ) ERROR STOP 55

                   IF ( LEN(p%C0) .NE.  513 ) ERROR STOP 56
                   IF ( LEN(p%C1) .NE. 1026 ) ERROR STOP 57
                   IF ( LEN(p%C2) .NE. 2052 ) ERROR STOP 58

                   IF ( SIZE(p%F0) .NE.  513 ) ERROR STOP 59
                   IF ( SIZE(p%F1) .NE. 1026 ) ERROR STOP 60
                   IF ( SIZE(p%F2) .NE. 2052 ) ERROR STOP 61

                   IF ( LBOUND(p%F0,1) .NE.  1 ) ERROR STOP 62
                   IF ( LBOUND(p%F1,1) .NE.  1 ) ERROR STOP 63
                   IF ( LBOUND(p%F2,1) .NE.  1 ) ERROR STOP 64

                   IF ( UBOUND(p%F0,1) .NE.  513 ) ERROR STOP 65
                   IF ( UBOUND(p%F1,1) .NE. 1026 ) ERROR STOP 66
                   IF ( UBOUND(p%F2,1) .NE. 2052 ) ERROR STOP 67

                   p%C0 = "K"
                   p%C1 = "L"
                   p%C2 = "M"
                   p%F0 = .True.
                   p%F1 = .True.
                   p%F2 = .True.

                 CLASS DEFAULT
                   STOP 122
              END SELECT
            END ASSOCIATE

          CLASS DEFAULT
            STOP 69
        END SELECT
      END SUBROUTINE
END MODULE

PROGRAM DTPMultipleNesting08
    USE Mod
    IMPLICIT CLASS(*)(U)
    IMPLICIT TYPE(NextGen1(4,513))(T)
    IMPLICIT TYPE(NextGen2(4,513))(C)
    POINTER :: U
    TARGET :: tgt, cbl

    U => tgt
    IF ( .NOT. ASSOCIATED(U) ) ERROR STOP 70
    CALL Select_type ( U )
    IF ( TRIM(tgt%C0) .NE. "A" ) ERROR STOP 71
    IF ( TRIM(tgt%C1) .NE. "B" ) ERROR STOP 72
    IF ( TRIM(tgt%C2) .NE. "C" ) ERROR STOP 73
    IF ( ANY(tgt%F0 .NEQV. .True.) ) ERROR STOP 74
    IF ( ANY(tgt%F1 .NEQV. .True.) ) ERROR STOP 75
    IF ( ANY(tgt%F2 .NEQV. .True.) ) ERROR STOP 76

    U => cbl
    IF ( .NOT. ASSOCIATED(U) ) ERROR STOP 80
    CALL Select_type ( U )
    IF ( TRIM(cbl%C0) .NE. "E" ) ERROR STOP 81
    IF ( TRIM(cbl%C1) .NE. "F" ) ERROR STOP 82
    IF ( TRIM(cbl%C2) .NE. "G" ) ERROR STOP 83
    IF ( ANY(cbl%F0 .NEQV. .False.) ) ERROR STOP 84
    IF ( ANY(cbl%F1 .NEQV. .False.) ) ERROR STOP 85
    IF ( ANY(cbl%F2 .NEQV. .False.) ) ERROR STOP 86

    ALLOCATE ( U, SOURCE = Branch(tgt,cbl) )
    IF ( .NOT. ASSOCIATED(U) ) ERROR STOP 90

    SELECT TYPE ( U )
       CLASS IS (Branch)
         IF ( .NOT. ASSOCIATED(U%cmp1, tgt) ) ERROR STOP 91
         IF ( .NOT. ASSOCIATED(U%cmp2, cbl) ) ERROR STOP 92

       CLASS DEFAULT
         STOP 122
    END SELECT

    CALL Select_type ( U )

    IF ( TRIM(tgt%C0) .NE. "H" ) ERROR STOP 93
    IF ( TRIM(tgt%C1) .NE. "I" ) ERROR STOP 94
    IF ( TRIM(tgt%C2) .NE. "J" ) ERROR STOP 95
    IF ( ANY(tgt%F0 .NEQV. .False.) ) ERROR STOP 96
    IF ( ANY(tgt%F1 .NEQV. .False.) ) ERROR STOP 97
    IF ( ANY(tgt%F2 .NEQV. .False.) ) ERROR STOP 98
    IF ( TRIM(cbl%C0) .NE. "K" ) ERROR STOP 99
    IF ( TRIM(cbl%C1) .NE. "L" ) ERROR STOP 100
    IF ( TRIM(cbl%C2) .NE. "M" ) ERROR STOP 101
    IF ( ANY(cbl%F0 .NEQV. .True.) ) ERROR STOP 102
    IF ( ANY(cbl%F1 .NEQV. .True.) ) ERROR STOP 103
    IF ( ANY(cbl%F2 .NEQV. .True.) ) ERROR STOP 104

END PROGRAM DTPMultipleNesting08
