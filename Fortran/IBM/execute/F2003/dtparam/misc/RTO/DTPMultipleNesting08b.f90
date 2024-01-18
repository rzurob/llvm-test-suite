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

            IF ( LEN(Arg%C1) .NE. 1026 ) ERROR STOP 12
            IF ( SIZE(Arg%F2) .NE. 2052 ) ERROR STOP 16
            IF ( LBOUND(Arg%F2,1) .NE.  1 ) ERROR STOP 19
            IF ( UBOUND(Arg%F2,1) .NE. 2052 ) ERROR STOP 22

            Arg%C2       = "C"
            Arg%F2       = .True.

          CLASS IS (NextGen2(4,*))
            IF ( Arg%l1 .NE. 513 ) ERROR STOP 23

            IF ( LEN(Arg%C2) .NE. 2052 ) ERROR STOP 26
            IF ( SIZE(Arg%F2) .NE. 2052 ) ERROR STOP 29
            IF ( LBOUND(Arg%F2,1) .NE.  1 ) ERROR STOP 32
            IF ( UBOUND(Arg%F2,1) .NE. 2052 ) ERROR STOP 35

            Arg%C2       = "G"
            Arg%F2       = .False.

          CLASS IS (Branch)

            ASSOCIATE ( p => Arg%cmp1 )
              SELECT TYPE ( p )
                 CLASS IS (NextGen1(4,*))
                   IF ( p%l1 .NE. 513 ) ERROR STOP 41

                   IF ( LEN(p%C2) .NE. 2052 ) ERROR STOP 44
                   IF ( SIZE(p%F2) .NE. 2052 ) ERROR STOP 47
                   IF ( LBOUND(p%F2,1) .NE.  1 ) ERROR STOP 50
                   IF ( UBOUND(p%F2,1) .NE. 2052 ) ERROR STOP 53

                   p%C2 = "J"
                   p%F2 = .False.

                 CLASS DEFAULT
                   STOP 121
              END SELECT
            END ASSOCIATE

            ASSOCIATE ( p => Arg%cmp2 )
              SELECT TYPE ( p )
                 CLASS IS (NextGen2(4,*))
                   IF ( p%l1 .NE. 513 ) ERROR STOP 55

                   IF ( LEN(p%C2) .NE. 2052 ) ERROR STOP 58
                   IF ( SIZE(p%F2) .NE. 2052 ) ERROR STOP 61
                   IF ( LBOUND(p%F2,1) .NE.  1 ) ERROR STOP 64
                   IF ( UBOUND(p%F2,1) .NE. 2052 ) ERROR STOP 67

                   p%C2 = "M"
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

PROGRAM DTPMultipleNesting08b
    USE Mod
    IMPLICIT CLASS(*)(U)
    IMPLICIT TYPE(NextGen1(4,513))(T)
    IMPLICIT TYPE(NextGen2(4,513))(C)
    POINTER :: U
    TARGET :: tgt, cbl

    U => tgt
    IF ( .NOT. ASSOCIATED(U) ) ERROR STOP 70
    CALL Select_type ( U )
    IF ( TRIM(tgt%C2) .NE. "C" ) ERROR STOP 73
    IF ( ANY(tgt%F2 .NEQV. .True.) ) ERROR STOP 76

    U => cbl
    IF ( .NOT. ASSOCIATED(U) ) ERROR STOP 80
    CALL Select_type ( U )
    IF ( TRIM(cbl%C2) .NE. "G" ) ERROR STOP 83
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

    IF ( TRIM(tgt%C2) .NE. "J" ) ERROR STOP 95
    IF ( ANY(tgt%F2 .NEQV. .False.) ) ERROR STOP 98
    IF ( TRIM(cbl%C2) .NE. "M" ) ERROR STOP 101
    IF ( ANY(cbl%F2 .NEQV. .True.) ) ERROR STOP 104

END PROGRAM DTPMultipleNesting08b
