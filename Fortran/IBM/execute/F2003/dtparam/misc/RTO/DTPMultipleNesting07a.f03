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
!* same as DTPMultipleNesting07 but accessing the most inner component without
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
            IF ( SIZE(Arg%F0) .NE.  513 ) ERROR STOP 12
            IF ( LBOUND(Arg%F0,1) .NE.  1 ) ERROR STOP 13
            IF ( UBOUND(Arg%F0,1) .NE.  513 ) ERROR STOP 14

            Arg%Child%C0 = "A"
            Arg%Child%F0 = .True.

          CLASS IS (Child(4,*))
            IF ( Arg%l1 .NE. 513 ) ERROR STOP 15
            IF ( LEN(Arg%C0) .NE.  513 ) ERROR STOP 16
            IF ( SIZE(Arg%F0) .NE.  513 ) ERROR STOP 17
            IF ( LBOUND(Arg%F0,1) .NE.  1 ) ERROR STOP 18
            IF ( UBOUND(Arg%F0,1) .NE.  513 ) ERROR STOP 19

            Arg%Base%C0  = "E"
            Arg%Base%F0  = .False.

          CLASS IS (Branch)

            IF ( .NOT. ASSOCIATED(Arg%cmp1, tgt1) ) ERROR STOP 20
            ASSOCIATE ( p => Arg%cmp1 )
              IF ( p%l1 .NE. 513 ) ERROR STOP 21
              SELECT TYPE ( p )
                 CLASS IS (NextGen1(4,*))
                    IF ( LEN(p%C0) .NE.  513 ) ERROR STOP 22
                    IF ( SIZE(p%F0) .NE.  513 ) ERROR STOP 23
                    IF ( LBOUND(p%F0,1) .NE.  1 ) ERROR STOP 24
                    IF ( UBOUND(p%F0,1) .NE.  513 ) ERROR STOP 25

                    p%C0 = "H"
                    p%F0 = .False.

                 CLASS DEFAULT
                   STOP 26
              END SELECT
            END ASSOCIATE

            IF ( .NOT. ASSOCIATED(Arg%cmp2, tgt2) ) ERROR STOP 30
            ASSOCIATE ( p => Arg%cmp2 )
              IF ( p%l1 .NE. 513 ) ERROR STOP 31
              SELECT TYPE ( p )
                 CLASS IS (Child(4,*))
                    IF ( LEN(p%C0) .NE.  513 ) ERROR STOP 32
                    IF ( SIZE(p%F0) .NE.  513 ) ERROR STOP 33
                    IF ( LBOUND(p%F0,1) .NE.  1 ) ERROR STOP 34
                    IF ( UBOUND(p%F0,1) .NE.  513 ) ERROR STOP 35

                    p%C0 = "K"
                    p%F0 = .True.

                 CLASS DEFAULT
                   STOP 36
              END SELECT
            END ASSOCIATE

          CLASS DEFAULT
            STOP 40
        END SELECT
      END SUBROUTINE
END MODULE

PROGRAM DTPMultipleNesting07a
    USE Mod
    IMPLICIT CLASS(*)(U)
    POINTER :: U

    U => tgt1
    IF ( .NOT. ASSOCIATED(U) ) ERROR STOP 50
    CALL Select_type ( U )
    IF ( TRIM(tgt1%C0) .NE. "A" ) ERROR STOP 51
    IF ( ANY(tgt1%F0 .NEQV. .True.) ) ERROR STOP 52

    U => tgt2
    IF ( .NOT. ASSOCIATED(U) ) ERROR STOP 53
    CALL Select_type ( U )
    IF ( TRIM(tgt2%C0) .NE. "E" ) ERROR STOP 54
    IF ( ANY(tgt2%F0 .NEQV. .False.) ) ERROR STOP 55

    ALLOCATE ( U, SOURCE = Branch(tgt1,tgt2) )
    IF ( .NOT. ASSOCIATED(U) ) ERROR STOP 56
    CALL Select_type ( U )
    IF ( TRIM(tgt1%C0) .NE. "H" ) ERROR STOP 57
    IF ( ANY(tgt1%F0 .NEQV. .False.) ) ERROR STOP 58
    IF ( TRIM(tgt2%C0) .NE. "K" ) ERROR STOP 59
    IF ( ANY(tgt2%F0 .NEQV. .True.) ) ERROR STOP 60

END PROGRAM DTPMultipleNesting07a
