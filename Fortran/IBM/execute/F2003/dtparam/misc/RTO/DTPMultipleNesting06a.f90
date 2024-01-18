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
!* same as DTPMultipleNesting06 but accessing the most inner component without
!* accessing the others
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
            IF ( SIZE(Arg%F0) .NE.  513 ) STOP 12
            IF ( LBOUND(Arg%F0,1) .NE.  1 ) STOP 13
            IF ( UBOUND(Arg%F0,1) .NE.  513 ) STOP 14

            Arg%Child%C0 = "A"
            Arg%Child%F0 = .True.

          CLASS IS (NextGen2(4,*))
            IF ( Arg%l1 .NE. 513 ) STOP 20
            IF ( LEN(Arg%C0) .NE.  513 ) STOP 21
            IF ( SIZE(Arg%F0) .NE.  513 ) STOP 22
            IF ( LBOUND(Arg%F0,1) .NE.  1 ) STOP 23
            IF ( UBOUND(Arg%F0,1) .NE.  513 ) STOP 24

            Arg%Base%C0  = "E"
            Arg%Base%F0  = .False.

          CLASS IS (Branch)

            IF ( .NOT. ASSOCIATED(Arg%cmp1, tgt1) ) STOP 30
            ASSOCIATE ( p => Arg%cmp1 )
              IF ( p%l1 .NE. 513 ) STOP 31
              IF ( LEN(p%C0) .NE.  513 ) STOP 32
              IF ( SIZE(p%F0) .NE.  513 ) STOP 33
              IF ( LBOUND(p%F0,1) .NE.  1 ) STOP 34
              IF ( UBOUND(p%F0,1) .NE.  513 ) STOP 35

              p%C0 = "H"
              p%F0 = .False.
            END ASSOCIATE

            IF ( .NOT. ASSOCIATED(Arg%cmp2, tgt2) ) STOP 36
            ASSOCIATE ( p => Arg%cmp2 )
              IF ( p%l1 .NE. 513 ) STOP 37
              IF ( LEN(p%C0) .NE.  513 ) STOP 38
              IF ( SIZE(p%F0) .NE.  513 ) STOP 39
              IF ( LBOUND(p%F0,1) .NE.  1 ) STOP 40
              IF ( UBOUND(p%F0,1) .NE.  513 ) STOP 41

              p%C0 = "K"
              p%F0 = .True.
            END ASSOCIATE

          CLASS DEFAULT
            STOP 42
        END SELECT
      END SUBROUTINE
END MODULE

PROGRAM DTPMultipleNesting06a
    USE Mod
    IMPLICIT CLASS(*)(U)
    POINTER :: U

    U => tgt1
    IF ( .NOT. ASSOCIATED(U) ) STOP 110

    CALL Select_type ( U )
    IF ( TRIM(tgt1%C0) .NE. "A" ) STOP 50
    IF ( ANY(tgt1%F0 .NEQV. .True.) ) STOP 51

    U => tgt2
    IF ( .NOT. ASSOCIATED(U) ) STOP 111

    CALL Select_type ( U )
    IF ( TRIM(tgt2%C0) .NE. "E" ) STOP 52
    IF ( ANY(tgt2%F0 .NEQV. .False.) ) STOP 53

    ALLOCATE ( U, SOURCE = Branch(tgt1,tgt2) )
    IF ( .NOT. ASSOCIATED(U) ) STOP 112

    CALL Select_type ( U )
    IF ( TRIM(tgt1%C0) .NE. "H" ) STOP 54
    IF ( ANY(tgt1%F0 .NEQV. .False.) ) STOP 55
    IF ( TRIM(tgt2%C0) .NE. "K" ) STOP 56
    IF ( ANY(tgt2%F0 .NEQV. .True.) ) STOP 99

END PROGRAM DTPMultipleNesting06a
