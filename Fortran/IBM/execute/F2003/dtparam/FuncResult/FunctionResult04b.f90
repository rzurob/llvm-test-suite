!*  ===================================================================
!*
!*  DATE                       : March 15, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Function result - unlimited poly
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
!* Defect 356663
!*
!234567890123456789012345678901234567890123456789012345678901234567890
MODULE Mod
      IMPLICIT NONE

      TYPE Base (k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN  :: l1

        CHARACTER(l1) :: C0
        INTEGER(k1), ALLOCATABLE :: A0(:)
      END TYPE

      TYPE,  EXTENDS(Base) :: Child (k2,l2)
        INTEGER, KIND :: k2
        INTEGER, LEN  :: l2
      END TYPE

      CONTAINS

      FUNCTION func(Arg)
        CLASS(Base(4,*)), INTENT(IN) :: Arg
        CLASS(Base(4,:)), ALLOCATABLE :: func

        SELECT TYPE ( Arg )
           CLASS IS (Base(4,*))
              ALLOCATE (Base(4,Arg%l1) :: func)
              func%C0 = Arg%C0
              func%A0 = Arg%A0

           CLASS IS (Child(4,*,4,*))
              ALLOCATE (Child(4,Arg%l1,4,Arg%l2) :: func)
              func%C0 = Arg%C0
              func%A0 = Arg%A0

           CLASS DEFAULT
              STOP 10
        END SELECT

      END FUNCTION
END MODULE

PROGRAM FunctionResult04b
      USE Mod
      IMPLICIT NONE

      TYPE(Base(4,10)) ::  b1
      INTEGER :: I

      b1 = func(Base(4,10) ( 'IBM', (/ (I, I = 1, 10) /) ))
      IF ( b1%C0     .NE.             'IBM' ) STOP 10
      IF ( ANY(b1%A0 .NE. [(I, I = 1, 10)]) ) STOP 11

      SELECT TYPE ( s => func(Child(4,5,4,5) ( 'XLFtest', (/ (I, I = 1, 5) /) )) )
         TYPE IS (Child(4,*,4,*))
            IF ( s%C0     .NE.        'XLFte' ) STOP 12
            IF ( ANY(s%A0 .NE. [(I, I = 1, 5)]) ) STOP 13

         CLASSDEFAULT
            STOP 11
      END SELECT

END PROGRAM FunctionResult04b
