!*  ===================================================================
!*
!*  DATE                       : April 24, 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : Explicit Init. Expression
!*  SECONDARY FUNCTIONS TESTED :
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

      TYPE,  EXTENDS(Child) :: NextGen (l3)
        INTEGER, LEN  :: l3

        INTEGER(k1)   :: A2(l3) = -3
        CHARACTER(l3) :: C2 = 'NextGen-init'
        TYPE(Base(k1,l3)) :: bcomp
        TYPE(Child(k1,l3,k1,l3)) :: ccomp
      END TYPE

      INTERFACE
         SUBROUTINE CreateNew(Arg)
            IMPORT BASE, CHILD, NEXTGEN
            CLASS(Base(4,*)) :: Arg
            CLASS(Base(4,:)), POINTER :: Obj
         END SUBROUTINE CreateNew
      END INTERFACE

END MODULE
PROGRAM ExplicitInitExp09
       USE Mod
       IMPLICIT NONE

       INTEGER :: I
       INTEGER, PARAMETER :: I10(10) = [(I, I = 1, 10)], I5(5) = [(I**2, I = 1, 5)]

       TYPE(Base(4,10))           :: b1 = Base(4,10) (I10, 'b1constant')
       TYPE(Child(4,10,4,5))      :: c1 = Child(4,10,4,5) (2*I10, 'c1const1', I5, 'c1const2')
       TYPE(NextGen(4,10,4,5,10)) :: n1 = NextGen(4,10,4,5,10)      &
                 (5, 'AB', 3, 'ABCDE', 1, 'n1constant', Base(4,10)(), Child(4,10,4,10)())

       CALL CreateNew(b1)

       CALL CreateNew(c1)

       CALL CreateNew(n1)

END PROGRAM ExplicitInitExp09
SUBROUTINE CreateNew(Arg)
       USE Mod, ONLY: BASE, CHILD, NEXTGEN
       CLASS(Base(4,*)), TARGET, INTENT(IN)  :: Arg
       CLASS(Base(4,:)), POINTER :: Obj

       Obj => Arg

       SELECT TYPE ( s => Obj )
         CLASS IS (Base(4,*))
            IF ( SIZE(s%A0)     .NE. 10 ) ERROR STOP 10
            IF ( LBOUND(s%A0,1) .NE.  1 ) ERROR STOP 11
            IF ( UBOUND(s%A0,1) .NE. 10 ) ERROR STOP 12
            IF ( LEN(s%C0)      .NE. 10 ) ERROR STOP 13
            IF ( ANY(s%A0   .NE. [(I, I = 1, 10)]) ) ERROR STOP 14
            IF ( TRIM(s%C0) .NE.      'b1constant' ) ERROR STOP 15

         CLASS IS (Child(4,*,4,*))
            IF ( SIZE(s%A0)     .NE. 10 ) ERROR STOP 16
            IF ( SIZE(s%A1)     .NE. 5 ) ERROR STOP 17
            IF ( LBOUND(s%A0,1) .NE. 1 ) ERROR STOP 18
            IF ( LBOUND(s%A1,1) .NE. 1 ) ERROR STOP 19
            IF ( UBOUND(s%A0,1) .NE. 10 ) ERROR STOP 20
            IF ( UBOUND(s%A1,1) .NE. 5 ) ERROR STOP 21
            IF ( LEN(s%C0)      .NE. 10 ) ERROR STOP 22
            IF ( LEN(s%C1)      .NE. 5 ) ERROR STOP 23
            IF ( ANY(s%A0   .NE. [(2*I, I = 1, 10)] ) ) ERROR STOP 24
            IF ( ANY(s%A1   .NE. [(I**2, I = 1, 5)] ) ) ERROR STOP 25
            IF ( TRIM(s%C0) .NE. 'c1const1' ) ERROR STOP 26
            IF ( TRIM(s%C1) .NE.    'c1con' ) ERROR STOP 27

         CLASS IS (NextGen(4,*,4,*,*))
            IF ( SIZE(s%A0)     .NE. 10 ) ERROR STOP 30
            IF ( SIZE(s%A1)     .NE.  5 ) ERROR STOP 31
            IF ( SIZE(s%A2)     .NE. 10 ) ERROR STOP 32
            IF ( LBOUND(s%A0,1) .NE.  1 ) ERROR STOP 33
            IF ( LBOUND(s%A1,1) .NE.  1 ) ERROR STOP 34
            IF ( LBOUND(s%A2,1) .NE.  1 ) ERROR STOP 35
            IF ( UBOUND(s%A0,1) .NE. 10 ) ERROR STOP 36
            IF ( UBOUND(s%A1,1) .NE.  5 ) ERROR STOP 37
            IF ( UBOUND(s%A2,1) .NE. 10 ) ERROR STOP 38
            IF ( LEN(s%C0)      .NE. 10 ) ERROR STOP 39
            IF ( LEN(s%C1)      .NE.  5 ) ERROR STOP 40
            IF ( LEN(s%C2)      .NE. 10 ) ERROR STOP 41
            IF ( ANY(s%A0   .NE. 5) ) ERROR STOP 42
            IF ( ANY(s%A1   .NE. 3) ) ERROR STOP 43
            IF ( ANY(s%A2   .NE. 1) ) ERROR STOP 44
            IF ( TRIM(s%C0) .NE.         'AB' ) ERROR STOP 45
            IF ( TRIM(s%C1) .NE.      'ABCDE' ) ERROR STOP 46
            IF ( TRIM(s%C2) .NE. 'n1constant' ) ERROR STOP 47

            IF ( SIZE(s%bcomp%A0)     .NE. 10 ) ERROR STOP 48
            IF ( LBOUND(s%bcomp%A0,1) .NE. 1 ) ERROR STOP 49
            IF ( UBOUND(s%bcomp%A0,1) .NE. 10 ) ERROR STOP 50
            IF ( LEN(s%bcomp%C0)      .NE. 10 ) ERROR STOP 51
            IF ( ANY(s%bcomp%A0   .NE.         -1) ) ERROR STOP 52
            IF ( TRIM(s%bcomp%C0) .NE. 'Base-init' ) ERROR STOP 53

            IF ( SIZE(s%ccomp%A0)     .NE. 10 ) ERROR STOP 54
            IF ( SIZE(s%ccomp%A1)     .NE. 10 ) ERROR STOP 55
            IF ( LBOUND(s%ccomp%A0,1) .NE. 1 ) ERROR STOP 56
            IF ( LBOUND(s%ccomp%A1,1) .NE. 1 ) ERROR STOP 57
            IF ( UBOUND(s%ccomp%A0,1) .NE. 10 ) ERROR STOP 58
            IF ( UBOUND(s%ccomp%A1,1) .NE. 10 ) ERROR STOP 59
            IF ( LEN(s%ccomp%C0)      .NE. 10 ) ERROR STOP 60
            IF ( LEN(s%ccomp%C1)      .NE. 10 ) ERROR STOP 61
            IF ( ANY(s%ccomp%A0   .NE.   -1) ) ERROR STOP 62
            IF ( ANY(s%ccomp%A1   .NE.   -2) ) ERROR STOP 63
            IF ( TRIM(s%ccomp%C0) .NE.  'Base-init' ) ERROR STOP 64
            IF ( TRIM(s%ccomp%C1) .NE. 'Child-init' ) ERROR STOP 65

         CLASS DEFAULT
            STOP 100
       END SELECT

END SUBROUTINE