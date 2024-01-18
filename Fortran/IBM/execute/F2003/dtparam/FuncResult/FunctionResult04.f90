!*  ===================================================================
!*
!*  DATE                       : March 15, 2008
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
!* Defect 363421
!*
!234567890123456789012345678901234567890123456789012345678901234567890
MODULE Mod
      IMPLICIT NONE

      TYPE Base (k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN  :: l1

        CHARACTER(l1) :: C0
        INTEGER(k1) :: A0(l1)
      END TYPE

      TYPE,  EXTENDS(Base) :: Child (k2,l2)
        INTEGER, KIND :: k2
        INTEGER, LEN  :: l2

      END TYPE

      CONTAINS

      FUNCTION func(b2)
        CLASS(Base(4,*)), INTENT(IN) :: b2
        CLASS(Base(4,:)), ALLOCATABLE :: func

        SELECT TYPE ( b2 )
           CLASS IS (Base(4,*))
              ALLOCATE (Base(4,b2%l1) :: func)
              func%C0 = b2%C0
              func%A0 = b2%A0

           CLASS IS (Child(4,*,4,*))
              ALLOCATE (Child(4,b2%l1,4,b2%l2) :: func)
              func%C0 = b2%C0
              func%A0 = b2%A0

           CLASS DEFAULT
              STOP 10
        END SELECT

      END FUNCTION
END MODULE

PROGRAM FunctionResult04
      USE Mod
      IMPLICIT NONE

      TYPE(Base(4,10)) ::  b1
      TYPE(Child(4,5,4,5)) ::  c1

      b1 = func(Base(4,10) ( 'IBM', 2 ))
      print*, b1
      IF ( b1%C0     .NE. 'IBM' ) STOP 10
      IF ( ANY(b1%A0 .NE.    2) ) STOP 11

      SELECT TYPE ( s => func(Child(4,5,4,5) ( 'XLF', 3 )) )
         TYPE IS (Child(4,*,4,*))
            IF ( s%C0     .NE. 'XLF' ) STOP 12
            IF ( ANY(s%A0 .NE.    3) ) STOP 13
            c1 = s
            print*, c1
            IF ( c1%C0     .NE. 'XLF' ) STOP 14
            IF ( ANY(c1%A0 .NE.    3) ) STOP 15

         CLASSDEFAULT
            STOP 11
      END SELECT

END PROGRAM FunctionResult04
