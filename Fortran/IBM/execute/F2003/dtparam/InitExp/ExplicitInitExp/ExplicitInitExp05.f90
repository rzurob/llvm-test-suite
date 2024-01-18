!*  ===================================================================
!*
!*  DATE                       : April 24, 2005
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Explicit Init. Exp.
!*  SECONDARY FUNCTIONS TESTED : SAVE attribute
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!* Defect 364814
!*
!234567890123456789012345678901234567890123456789012345678901234567890
MODULE Mod
      IMPLICIT NONE

      TYPE Base (k1,l1)
        INTEGER, KIND :: k1 = 4
        INTEGER, LEN  :: l1

        INTEGER(k1)   :: A0(l1) = -1
        CHARACTER(l1) :: C0 = 'Base-init'
      END TYPE

      TYPE, EXTENDS(Base) :: Child (k2,l2)
        INTEGER, KIND :: k2 = 4
        INTEGER, LEN  :: l2

        INTEGER(k1)   :: A1(l2) = -2
        CHARACTER(l2) :: C1 = 'Child-init'
      END TYPE

      TYPE,  EXTENDS(Child) :: GrandChild (l3)
        INTEGER, LEN  :: l3

        INTEGER(k1)   :: A2(l3) = -3
        CHARACTER(l3) :: C2 = 'GrandChild-init'
        TYPE(Base(k1,l3)) :: bcomp
        TYPE(Child(k1,l3,k1,l3)) :: ccomp
      END TYPE

      INTEGER :: I
      INTEGER, PARAMETER :: I10(10) = [(I, I = 1, 10)], I5(5) = [(I**2, I = 1, 5)]

      CONTAINS

      SUBROUTINE CreateNewBase(Arg)
       CLASS(Base(4,:)), POINTER  :: Arg
       TYPE(Base(4,10)), TARGET   :: b1 = Base(4,10) (I10, 'b1constant')  ! b1 has the SAVE attribute

       Arg => b1

      END SUBROUTINE

      SUBROUTINE CreateNewChild(Arg)
       CLASS(Base(4,:)), POINTER     :: Arg
       TYPE(Child(4,10,4,5)), TARGET :: c1 = Child(4,10,4,5) (2*I10, 'c1const1', I5, 'c1const2')

       Arg => c1

      END SUBROUTINE

      SUBROUTINE CreateNewGrandChild(Arg)
       CLASS(Base(4,:)), POINTER             :: Arg
       TYPE(GrandChild(4,10,4,5,10)), TARGET :: n1 = GrandChild(4,10,4,5,10)      &
                 (5, 'AB', 3, 'ABCDE', 1, 'n1constant', Base(4,10)(), Child(4,10,4,10)())

       Arg => n1

      END SUBROUTINE

END MODULE
PROGRAM ExplicitInitExp05
       USE Mod
       IMPLICIT NONE
       CLASS(Base(4,:)), POINTER  :: ptr => NULL()

       CALL CreateNewBase(ptr)
       IF ( .NOT. ASSOCIATED(ptr) ) STOP 10
       SELECT TYPE ( ptr )
         CLASS IS (Base(4,*))
            IF ( ptr%k1 .NE.  4 ) STOP 11
            IF ( ptr%l1 .NE. 10 ) STOP 12
            IF ( SIZE(ptr%A0)     .NE. 10 ) STOP 13
            IF ( LBOUND(ptr%A0,1) .NE.  1 ) STOP 14
            IF ( UBOUND(ptr%A0,1) .NE. 10 ) STOP 15
            IF ( LEN(ptr%C0)      .NE. 10 ) STOP 16
            IF ( ANY(ptr%A0   .NE. [(I, I = 1, 10)]) ) STOP 17
            IF ( TRIM(ptr%C0) .NE.      'b1constant' ) STOP 18

         CLASS DEFAULT
            STOP 19
       END SELECT

       CALL CreateNewChild(ptr)
       IF ( .NOT. ASSOCIATED(ptr) ) STOP 20
       SELECT TYPE ( ptr )
         CLASS IS (Child(4,*,4,*))
            IF ( ptr%k1 .NE.  4 ) STOP 21
            IF ( ptr%l1 .NE. 10 ) STOP 22
            IF ( ptr%k2 .NE.  4 ) STOP 23
            IF ( ptr%l2 .NE.  5 ) STOP 24
            IF ( SIZE(ptr%A0)     .NE. 10 ) STOP 25
            IF ( SIZE(ptr%A1)     .NE.  5 ) STOP 26
            IF ( LBOUND(ptr%A0,1) .NE.  1 ) STOP 27
            IF ( LBOUND(ptr%A1,1) .NE.  1 ) STOP 28
            IF ( UBOUND(ptr%A0,1) .NE. 10 ) STOP 29
            IF ( UBOUND(ptr%A1,1) .NE.  5 ) STOP 30
            IF ( LEN(ptr%C0)      .NE. 10 ) STOP 31
            IF ( LEN(ptr%C1)      .NE.  5 ) STOP 32
            IF ( ANY(ptr%A0   .NE. [(2*I, I = 1, 10)] ) ) STOP 33
            IF ( ANY(ptr%A1   .NE. [(I**2, I = 1, 5)] ) ) STOP 34
            IF ( TRIM(ptr%C0) .NE. 'c1const1' ) STOP 35
            IF ( TRIM(ptr%C1) .NE.    'c1con' ) STOP 36

         CLASS DEFAULT
            STOP 37
       END SELECT

       CALL CreateNewGrandChild(ptr)
       IF ( .NOT. ASSOCIATED(ptr) ) STOP 40
       SELECT TYPE ( ptr )
         CLASS IS (GrandChild(4,*,4,*,*))
            IF ( ptr%k1 .NE.  4 ) STOP 41
            IF ( ptr%l1 .NE. 10 ) STOP 42
            IF ( ptr%k2 .NE.  4 ) STOP 43
            IF ( ptr%l2 .NE.  5 ) STOP 44
            IF ( ptr%l3 .NE. 10 ) STOP 45
            IF ( SIZE(ptr%A0)     .NE. 10 ) STOP 46
            IF ( SIZE(ptr%A1)     .NE.  5 ) STOP 47
            IF ( SIZE(ptr%A2)     .NE. 10 ) STOP 48
            IF ( LBOUND(ptr%A0,1) .NE.  1 ) STOP 49
            IF ( LBOUND(ptr%A1,1) .NE.  1 ) STOP 50
            IF ( LBOUND(ptr%A2,1) .NE.  1 ) STOP 51
            IF ( UBOUND(ptr%A0,1) .NE. 10 ) STOP 52
            IF ( UBOUND(ptr%A1,1) .NE.  5 ) STOP 53
            IF ( UBOUND(ptr%A2,1) .NE. 10 ) STOP 54
            IF ( LEN(ptr%C0)      .NE. 10 ) STOP 55
            IF ( LEN(ptr%C1)      .NE.  5 ) STOP 56
            IF ( LEN(ptr%C2)      .NE. 10 ) STOP 57
            IF ( ANY(ptr%A0   .NE. 5) ) STOP 58
            IF ( ANY(ptr%A1   .NE. 3) ) STOP 59
            IF ( ANY(ptr%A2   .NE. 1) ) STOP 60
            IF ( TRIM(ptr%C0) .NE.         'AB' ) STOP 61
            IF ( TRIM(ptr%C1) .NE.      'ABCDE' ) STOP 62
            IF ( TRIM(ptr%C2) .NE. 'n1constant' ) STOP 63

            IF ( ptr%bcomp%k1 .NE.  4 ) STOP 64
            IF ( ptr%bcomp%l1 .NE. 10 ) STOP 65
            IF ( SIZE(ptr%bcomp%A0)     .NE. 10 ) STOP 66
            IF ( LBOUND(ptr%bcomp%A0,1) .NE.  1 ) STOP 67
            IF ( UBOUND(ptr%bcomp%A0,1) .NE. 10 ) STOP 68
            IF ( LEN(ptr%bcomp%C0)      .NE. 10 ) STOP 69
            IF ( ANY(ptr%bcomp%A0   .NE.         -1) ) STOP 70
            IF ( TRIM(ptr%bcomp%C0) .NE. 'Base-init' ) STOP 71

            IF ( ptr%ccomp%k1 .NE.  4 ) STOP 72
            IF ( ptr%ccomp%l1 .NE. 10 ) STOP 73
            IF ( ptr%ccomp%k2 .NE.  4 ) STOP 74
            IF ( ptr%ccomp%l2 .NE. 10 ) STOP 75
            IF ( SIZE(ptr%ccomp%A0)     .NE. 10 ) STOP 76
            IF ( SIZE(ptr%ccomp%A1)     .NE. 10 ) STOP 77
            IF ( LBOUND(ptr%ccomp%A0,1) .NE.  1 ) STOP 78
            IF ( LBOUND(ptr%ccomp%A1,1) .NE.  1 ) STOP 79
            IF ( UBOUND(ptr%ccomp%A0,1) .NE. 10 ) STOP 80
            IF ( UBOUND(ptr%ccomp%A1,1) .NE. 10 ) STOP 81
            IF ( LEN(ptr%ccomp%C0)      .NE. 10 ) STOP 82
            IF ( LEN(ptr%ccomp%C1)      .NE. 10 ) STOP 83
            IF ( ANY(ptr%ccomp%A0   .NE.   -1) ) STOP 84
            IF ( ANY(ptr%ccomp%A1   .NE.   -2) ) STOP 85
            IF ( TRIM(ptr%ccomp%C0) .NE.  'Base-init' ) STOP 86
            IF ( TRIM(ptr%ccomp%C1) .NE. 'Child-init' ) STOP 87

         CLASS DEFAULT
            STOP 88
       END SELECT

END PROGRAM ExplicitInitExp05
