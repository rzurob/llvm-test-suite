!*  ===================================================================
!*
!*  DATE                       : April 24, 2009
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Named Constant (PARAMETER)
!*  SECONDARY FUNCTIONS TESTED : Automatic Object
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*  Defect: 364814
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

      CONTAINS

      SUBROUTINE CreateNewBase(Arg)
        TYPE(Base(4,*)) :: Arg
        TYPE(Base(4,Arg%l1)) :: Obj
        INTEGER :: I

        Obj = Arg
        IF ( SIZE(Obj%A0)     .NE. 10 ) STOP 10
        IF ( LBOUND(Obj%A0,1) .NE.  1 ) STOP 11
        IF ( UBOUND(Obj%A0,1) .NE. 10 ) STOP 12
        IF ( LEN(Obj%C0)      .NE. 10 ) STOP 13
        IF ( ANY(Obj%A0   .NE. [(I, I = 1, 10)]) ) STOP 14
        IF ( TRIM(Obj%C0) .NE.      'b1constant' ) STOP 15
      END SUBROUTINE

      SUBROUTINE CreateNewChild(Arg)
        TYPE(Child(4,*,4,*)) :: Arg
        TYPE(Child(4,Arg%l1,4,Arg%l2)) :: Obj
        INTEGER :: I

        Obj = Arg
        IF ( SIZE(Obj%A0)     .NE. 10 ) STOP 16
        IF ( SIZE(Obj%A1)     .NE.  5 ) STOP 17
        IF ( LBOUND(Obj%A0,1) .NE.  1 ) STOP 18
        IF ( LBOUND(Obj%A1,1) .NE.  1 ) STOP 19
        IF ( UBOUND(Obj%A0,1) .NE. 10 ) STOP 20
        IF ( UBOUND(Obj%A1,1) .NE.  5 ) STOP 21
        IF ( LEN(Obj%C0)      .NE. 10 ) STOP 22
        IF ( LEN(Obj%C1)      .NE.  5 ) STOP 23
        IF ( ANY(Obj%A0   .NE. [(2*I, I = 1, 10)] ) ) STOP 24
        IF ( ANY(Obj%A1   .NE. [(I**2, I = 1, 5)] ) ) STOP 25
        IF ( TRIM(Obj%C0) .NE. 'c1const1' ) STOP 26
        IF ( TRIM(Obj%C1) .NE.    'c1con' ) STOP 27
      END SUBROUTINE


      SUBROUTINE CreateNewNextGen(Arg)
        TYPE(NextGen(4,*,4,*,*)) :: Arg
        TYPE(NextGen(4,Arg%l1,4,Arg%l2,Arg%l3)) :: Obj
        INTEGER :: I

        Obj = Arg
        IF ( SIZE(Obj%A0)     .NE. 10 ) STOP 30
        IF ( SIZE(Obj%A1)     .NE.  5 ) STOP 31
        IF ( SIZE(Obj%A2)     .NE. 10 ) STOP 32
        IF ( LBOUND(Obj%A0,1) .NE.  1 ) STOP 33
        IF ( LBOUND(Obj%A1,1) .NE.  1 ) STOP 34
        IF ( LBOUND(Obj%A2,1) .NE.  1 ) STOP 35
        IF ( UBOUND(Obj%A0,1) .NE. 10 ) STOP 36
        IF ( UBOUND(Obj%A1,1) .NE.  5 ) STOP 37
        IF ( UBOUND(Obj%A2,1) .NE. 10 ) STOP 38
        IF ( LEN(Obj%C0)      .NE. 10 ) STOP 39
        IF ( LEN(Obj%C1)      .NE.  5 ) STOP 40
        IF ( LEN(Obj%C2)      .NE. 10 ) STOP 41
        IF ( ANY(Obj%A0   .NE. 5) ) STOP 42
        IF ( ANY(Obj%A1   .NE. 3) ) STOP 43
        IF ( ANY(Obj%A2   .NE. 1) ) STOP 44
        IF ( TRIM(Obj%C0) .NE.         'AB' ) STOP 45
        IF ( TRIM(Obj%C1) .NE.      'ABCDE' ) STOP 46
        IF ( TRIM(Obj%C2) .NE. 'n1constant' ) STOP 47

        IF ( SIZE(Obj%bcomp%A0)     .NE. 10 ) STOP 48
        IF ( LBOUND(Obj%bcomp%A0,1) .NE. 1 ) STOP 49
        IF ( UBOUND(Obj%bcomp%A0,1) .NE. 10 ) STOP 50
        IF ( LEN(Obj%bcomp%C0)      .NE. 10 ) STOP 51
        IF ( ANY(Obj%bcomp%A0   .NE.         -1) ) STOP 52
        IF ( TRIM(Obj%bcomp%C0) .NE. 'Base-init' ) STOP 53

        IF ( SIZE(Obj%ccomp%A0)     .NE. 10 ) STOP 54
        IF ( SIZE(Obj%ccomp%A1)     .NE. 10 ) STOP 55
        IF ( LBOUND(Obj%ccomp%A0,1) .NE. 1 ) STOP 56
        IF ( LBOUND(Obj%ccomp%A1,1) .NE. 1 ) STOP 57
        IF ( UBOUND(Obj%ccomp%A0,1) .NE. 10 ) STOP 58
        IF ( UBOUND(Obj%ccomp%A1,1) .NE. 10 ) STOP 59
        IF ( LEN(Obj%ccomp%C0)      .NE. 10 ) STOP 60
        IF ( LEN(Obj%ccomp%C1)      .NE. 10 ) STOP 61
        IF ( ANY(Obj%ccomp%A0   .NE.    -1) ) STOP 62
        IF ( ANY(Obj%ccomp%A1   .NE.    -2) ) STOP 63
        IF ( TRIM(Obj%ccomp%C0) .NE.  'Base-init' ) STOP 64
        IF ( TRIM(Obj%ccomp%C1) .NE. 'Child-init' ) STOP 65
      END SUBROUTINE

END MODULE
PROGRAM DTP_PARAMETER_08b
      USE Mod
      IMPLICIT NONE

      INTEGER :: I
      INTEGER, PARAMETER :: I10(10) = [(I, I = 1, 10)], I5(5) = [(I**2, I = 1, 5)]

      TYPE(Base(4,10)),              PARAMETER :: b1 = Base(4,10)             &
                 ( I10, 'b1constant' )
      TYPE(Child(4,10,4,5)),        PARAMETER :: c1 = Child(4,10,4,5)         &
                 ( 2*I10, 'c1const1', I5, 'c1const2' )
      TYPE(NextGen(4,10,4,5,10)), PARAMETER :: n1 = NextGen(4,10,4,5,10)      &
                 ( 5, 'AB', 3, 'ABCDE', 1, 'n1constant', Base(4,10)(), Child(4,10,4,10)() )

      CALL CreateNewBase(b1)

      CALL CreateNewChild(c1)

      CALL CreateNewNextGen(n1)

END PROGRAM DTP_PARAMETER_08b
