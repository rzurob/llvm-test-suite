!*  ===================================================================
!*
!*  DATE                       : April 24, 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : Named Constant (PARAMETER)
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
!*  Defect: 355942
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
        TYPE(Base(4,:)), ALLOCATABLE :: Obj
        INTEGER :: I

        Obj = Arg
        IF ( SIZE(Obj%A0)     .NE. 10 ) ERROR STOP 10
        IF ( LBOUND(Obj%A0,1) .NE.  1 ) ERROR STOP 11
        IF ( UBOUND(Obj%A0,1) .NE. 10 ) ERROR STOP 12
        IF ( LEN(Obj%C0)      .NE. 10 ) ERROR STOP 13
        IF ( ANY(Obj%A0   .NE. [(I, I = 1, 10)]) ) ERROR STOP 14
        IF ( TRIM(Obj%C0) .NE.      'b1constant' ) ERROR STOP 15
      END SUBROUTINE

      SUBROUTINE CreateNewChild(Arg)
        TYPE(Child(4,*,4,*)) :: Arg
        TYPE(Child(4,:,4,:)), ALLOCATABLE :: Obj
        INTEGER :: I

        Obj = Arg
        IF ( SIZE(Obj%A0)     .NE. 10 ) ERROR STOP 16
        IF ( SIZE(Obj%A1)     .NE.  5 ) ERROR STOP 17
        IF ( LBOUND(Obj%A0,1) .NE.  1 ) ERROR STOP 18
        IF ( LBOUND(Obj%A1,1) .NE.  1 ) ERROR STOP 19
        IF ( UBOUND(Obj%A0,1) .NE. 10 ) ERROR STOP 20
        IF ( UBOUND(Obj%A1,1) .NE.  5 ) ERROR STOP 21
        IF ( LEN(Obj%C0)      .NE. 10 ) ERROR STOP 22
        IF ( LEN(Obj%C1)      .NE.  5 ) ERROR STOP 23
        IF ( ANY(Obj%A0   .NE. [(2*I, I = 1, 10)] ) ) ERROR STOP 24
        IF ( ANY(Obj%A1   .NE. [(I**2, I = 1, 5)] ) ) ERROR STOP 25
        IF ( TRIM(Obj%C0) .NE. 'c1const1' ) ERROR STOP 26
        IF ( TRIM(Obj%C1) .NE.    'c1con' ) ERROR STOP 27
      END SUBROUTINE


      SUBROUTINE CreateNewNextGen(Arg)
        TYPE(NextGen(4,*,4,*,*)) :: Arg
        TYPE(NextGen(4,:,4,:,:)), ALLOCATABLE :: Obj
        INTEGER :: I

        Obj = Arg
        IF ( SIZE(Obj%A0)     .NE. 10 ) ERROR STOP 30
        IF ( SIZE(Obj%A1)     .NE.  5 ) ERROR STOP 31
        IF ( SIZE(Obj%A2)     .NE. 10 ) ERROR STOP 32
        IF ( LBOUND(Obj%A0,1) .NE.  1 ) ERROR STOP 33
        IF ( LBOUND(Obj%A1,1) .NE.  1 ) ERROR STOP 34
        IF ( LBOUND(Obj%A2,1) .NE.  1 ) ERROR STOP 35
        IF ( UBOUND(Obj%A0,1) .NE. 10 ) ERROR STOP 36
        IF ( UBOUND(Obj%A1,1) .NE.  5 ) ERROR STOP 37
        IF ( UBOUND(Obj%A2,1) .NE. 10 ) ERROR STOP 38
        IF ( LEN(Obj%C0)      .NE. 10 ) ERROR STOP 39
        IF ( LEN(Obj%C1)      .NE.  5 ) ERROR STOP 40
        IF ( LEN(Obj%C2)      .NE. 10 ) ERROR STOP 41
        IF ( ANY(Obj%A0   .NE. 5) ) ERROR STOP 42
        IF ( ANY(Obj%A1   .NE. 3) ) ERROR STOP 43
        IF ( ANY(Obj%A2   .NE. 1) ) ERROR STOP 44
        IF ( TRIM(Obj%C0) .NE.         'AB' ) ERROR STOP 45
        IF ( TRIM(Obj%C1) .NE.      'ABCDE' ) ERROR STOP 46
        IF ( TRIM(Obj%C2) .NE. 'n1constant' ) ERROR STOP 47

        IF ( SIZE(Obj%bcomp%A0)     .NE. 10 ) ERROR STOP 48
        IF ( LBOUND(Obj%bcomp%A0,1) .NE. 1 ) ERROR STOP 49
        IF ( UBOUND(Obj%bcomp%A0,1) .NE. 10 ) ERROR STOP 50
        IF ( LEN(Obj%bcomp%C0)      .NE. 10 ) ERROR STOP 51
        IF ( ANY(Obj%bcomp%A0   .NE.         -1) ) ERROR STOP 52
        IF ( TRIM(Obj%bcomp%C0) .NE. 'Base-init' ) ERROR STOP 53

        IF ( SIZE(Obj%ccomp%A0)     .NE. 10 ) ERROR STOP 54
        IF ( SIZE(Obj%ccomp%A1)     .NE. 10 ) ERROR STOP 55
        IF ( LBOUND(Obj%ccomp%A0,1) .NE. 1 ) ERROR STOP 56
        IF ( LBOUND(Obj%ccomp%A1,1) .NE. 1 ) ERROR STOP 57
        IF ( UBOUND(Obj%ccomp%A0,1) .NE. 10 ) ERROR STOP 58
        IF ( UBOUND(Obj%ccomp%A1,1) .NE. 10 ) ERROR STOP 59
        IF ( LEN(Obj%ccomp%C0)      .NE. 10 ) ERROR STOP 60
        IF ( LEN(Obj%ccomp%C1)      .NE. 10 ) ERROR STOP 61
        IF ( ANY(Obj%ccomp%A0   .NE.    -1) ) ERROR STOP 62
        IF ( ANY(Obj%ccomp%A1   .NE.    -2) ) ERROR STOP 63
        IF ( TRIM(Obj%ccomp%C0) .NE.  'Base-init' ) ERROR STOP 64
        IF ( TRIM(Obj%ccomp%C1) .NE. 'Child-init' ) ERROR STOP 65
      END SUBROUTINE

END MODULE
PROGRAM DTP_PARAMETER_07b
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

END PROGRAM DTP_PARAMETER_07b