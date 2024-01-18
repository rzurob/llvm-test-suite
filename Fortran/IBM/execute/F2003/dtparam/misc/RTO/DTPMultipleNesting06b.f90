!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : DTPMultipleNesting06b
!*
!*  PROGRAMMER                 : Dorra Bouchiha
!*  DATE                       : February 05, 2008
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
!* same as DTPMultipleNesting06b but accessing the second inner most component 
!* without accessing the others
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

            IF ( .NOT. ASSOCIATED(Arg%cmp1, tgt1) ) STOP 20 
            ASSOCIATE ( p => Arg%cmp1 )
              IF ( p%l1 .NE. 513 ) STOP 21 
    
              IF ( LEN(p%C1) .NE. 1026 ) STOP 22
              IF ( SIZE(p%F1) .NE. 1026 ) STOP 23
              IF ( LBOUND(p%F1,1) .NE.  1 ) STOP 24
              IF ( UBOUND(p%F1,1) .NE. 1026 ) STOP 25

              p%C1 = "I"
              p%F1 = .False.
            END ASSOCIATE

            IF ( .NOT. ASSOCIATED(Arg%cmp2, tgt2) ) STOP 26 
            ASSOCIATE ( p => Arg%cmp2 )
              IF ( p%l1 .NE. 513 ) STOP 27 
    
              IF ( LEN(p%C1) .NE. 1026 ) STOP 28 
              IF ( SIZE(p%F1) .NE. 1026 ) STOP 29 
              IF ( LBOUND(p%F1,1) .NE.  1 ) STOP 30 
              IF ( UBOUND(p%F1,1) .NE. 1026 ) STOP 31 

              p%C1 = "L"
              p%F1 = .True. 
            END ASSOCIATE
    
          CLASS DEFAULT
            STOP 32 
        END SELECT
      END SUBROUTINE 
END MODULE

PROGRAM DTPMultipleNesting06b
    USE Mod
    IMPLICIT CLASS(*)(U)
    POINTER :: U

    U => tgt1
    IF ( .NOT. ASSOCIATED(U) ) STOP 40 

    CALL Select_type ( U )
    IF ( TRIM(tgt1%C1) .NE. "B" ) STOP 41
    IF ( ANY(tgt1%F1 .NEQV. .True.) ) STOP 42

    U => tgt2
    IF ( .NOT. ASSOCIATED(U) ) STOP 43  
    CALL Select_type ( U )
    IF ( TRIM(tgt2%C1) .NE. "F" ) STOP 44 
    IF ( ANY(tgt2%F1 .NEQV. .False.) ) STOP 45

    ALLOCATE ( U, SOURCE = Branch(tgt1,tgt2) )
    IF ( .NOT. ASSOCIATED(U) ) STOP 46
    CALL Select_type ( U )
    IF ( TRIM(tgt1%C1) .NE. "I" ) STOP 47 
    IF ( ANY(tgt1%F1 .NEQV. .False.) ) STOP 48
    IF ( TRIM(tgt2%C1) .NE. "L" ) STOP 49 
    IF ( ANY(tgt2%F1 .NEQV. .True.) ) STOP 50

END PROGRAM DTPMultipleNesting06b
