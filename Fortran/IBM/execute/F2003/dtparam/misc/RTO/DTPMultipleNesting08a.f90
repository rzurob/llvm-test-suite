!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : DTPMultipleNesting08a
!*
!*  PROGRAMMER                 : Dorra Bouchiha
!*  DATE                       : February 15, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Run Time Offset (RTO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*
!*  DRIVER STANZA              : xlf2003
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
            IF ( Arg%l1 .NE. 513 ) STOP 10
    
            IF ( LEN(Arg%C1) .NE. 1026 ) STOP 11
            IF ( SIZE(Arg%F1) .NE. 1026 ) STOP 12
            IF ( LBOUND(Arg%F1,1) .NE.  1 ) STOP 13
            IF ( UBOUND(Arg%F1,1) .NE. 1026 ) STOP 14
    
            Arg%Child%C1 = "B"
            Arg%Child%F1 = .True.
    
          CLASS IS (NextGen2(4,*))
            IF ( Arg%l1 .NE. 513 ) STOP 15
    
            IF ( LEN(Arg%C1) .NE. 1026 ) STOP 16
            IF ( SIZE(Arg%F1) .NE. 1026 ) STOP 17
            IF ( LBOUND(Arg%F1,1) .NE.  1 ) STOP 18
            IF ( UBOUND(Arg%F1,1) .NE. 1026 ) STOP 19
    
            Arg%Child%C1 = "F"
            Arg%Child%F1 = .False.

          CLASS IS (Branch)

            ASSOCIATE ( p => Arg%cmp1 )
              SELECT TYPE ( p )
                 CLASS IS (NextGen1(4,*))
                   IF ( p%l1 .NE. 513 ) STOP 20

                   IF ( LEN(p%C1) .NE. 1026 ) STOP 21
                   IF ( SIZE(p%F1) .NE. 1026 ) STOP 22
                   IF ( LBOUND(p%F1,1) .NE.  1 ) STOP 23
                   IF ( UBOUND(p%F1,1) .NE. 1026 ) STOP 24

                   p%C1 = "I"
                   p%F1 = .False.

                 CLASS DEFAULT
                   STOP 25
              END SELECT
            END ASSOCIATE

            ASSOCIATE ( p => Arg%cmp2 )
              SELECT TYPE ( p )
                 CLASS IS (NextGen2(4,*))
                   IF ( p%l1 .NE. 513 ) STOP 30 

                   IF ( LEN(p%C1) .NE. 1026 ) STOP 31
                   IF ( SIZE(p%F1) .NE. 1026 ) STOP 32
                   IF ( LBOUND(p%F1,1) .NE.  1 ) STOP 33
                   IF ( UBOUND(p%F1,1) .NE. 1026 ) STOP 34

                   p%C1 = "L"
                   p%F1 = .True. 

                 CLASS DEFAULT
                   STOP 35
              END SELECT
            END ASSOCIATE
    
          CLASS DEFAULT
            STOP 36
        END SELECT
      END SUBROUTINE
END MODULE

PROGRAM DTPMultipleNesting08a
    USE Mod
    IMPLICIT CLASS(*)(U)
    IMPLICIT TYPE(NextGen1(4,513))(T)
    IMPLICIT TYPE(NextGen2(4,513))(C)
    POINTER :: U
    TARGET :: tgt, cbl

    U => tgt
    IF ( .NOT. ASSOCIATED(U) ) STOP 40
    CALL Select_type ( U )
    IF ( TRIM(tgt%C1) .NE. "B" ) STOP 41
    IF ( ANY(tgt%F1 .NEQV. .True.) ) STOP 42

    U => cbl
    IF ( .NOT. ASSOCIATED(U) ) STOP 43
    CALL Select_type ( U )
    IF ( TRIM(cbl%C1) .NE. "F" ) STOP 44
    IF ( ANY(cbl%F1 .NEQV. .False.) ) STOP 45

    ALLOCATE ( U, SOURCE = Branch(tgt,cbl) )
    IF ( .NOT. ASSOCIATED(U) ) STOP 46

    SELECT TYPE ( U )
       CLASS IS (Branch)
         IF ( .NOT. ASSOCIATED(U%cmp1, tgt) ) STOP 47
         IF ( .NOT. ASSOCIATED(U%cmp2, cbl) ) STOP 48

       CLASS DEFAULT
         STOP 49
    END SELECT

    CALL Select_type ( U )

    IF ( TRIM(tgt%C1) .NE. "I" ) STOP 50
    IF ( ANY(tgt%F1 .NEQV. .False.) ) STOP 51
    IF ( TRIM(cbl%C1) .NE. "L" ) STOP 52
    IF ( ANY(cbl%F1 .NEQV. .True.) ) STOP 53

END PROGRAM DTPMultipleNesting08a
