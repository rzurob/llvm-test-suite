!*  ===================================================================
!*
!*  DATE                       : March 25, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : Function result
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
!234567890123456789012345678901234567890123456789012345678901234567890
MODULE Mod
      TYPE :: Base(k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN :: l1

        CHARACTER(LEN=l1) :: tag = "Niels"
        INTEGER(k1) :: arr(l1) = -1

        CONTAINS
        PROCEDURE :: print => printBase
      END TYPE

      TYPE,  EXTENDS(Base) :: Child (k2,l2)
        INTEGER, KIND :: k2
        INTEGER, LEN  :: l2

        INTEGER(k2) :: A0(l2), A1(l1), A2(l1+l2)

        CONTAINS
        PROCEDURE :: print => printChild
      END TYPE

      CONTAINS

      FUNCTION foo(Arg) RESULT(ReturnVal)
        CLASS(Base(4,*)), INTENT(IN) :: Arg
        CLASS(Base(4,:)), ALLOCATABLE  :: ReturnVal

         ALLOCATE( ReturnVal , SOURCE = Arg )
      END FUNCTION

      SUBROUTINE printBase (arg)
        CLASS(Base(4,*)), INTENT(IN) :: arg

        print *, arg%tag
        print *, arg%arr
      END SUBROUTINE

      SUBROUTINE printChild (arg)
        CLASS(Child(4,*,4,*)), INTENT(IN) :: arg

        print *, arg%tag
        print *, arg%arr
        print *, arg%A0
        print *, arg%A1
        print *, arg%A2
      END SUBROUTINE


END MODULE
PROGRAM FunctionResult13
      USE Mod
      IMPLICIT NONE
      INTEGER :: I
      TYPE(Base(4,5)) :: b1, b2 = Base(4,5)( "Bohr", (/ (I, I = 1, 5) /) )
      TYPE(Child(4,5,4,10)) :: c1
      CLASS(Base(4,:)), POINTER :: ptr


      call ASSOCIATE1 ( a = foo(b1) )

      call ASSOCIATE2 ( a = foo(b2) )

      call ASSOCIATE3 ( a = foo(Base(4,6)( "Henrik", (/ (I**2, I = 1, 6) /) )) )

      ALLOCATE ( ptr, SOURCE = Base(4,11) ("Schrodinger", -99) )

      call ASSOCIATE4 ( a = foo(ptr) )

      c1%Base = b2 ; c1%A0 = (/ (I, I = 1, 10) /); c1%A1 = (/ (I, I = 1, 5) /); c1%A2 = (/ (I, I = 1, 15) /)

      SELECT TYPE ( a => foo(c1) )
        CLASS IS (Child(4,*,4,*))
          call a%print
          IF ( a%tag .NE. "Bohr" ) STOP 20
          IF ( ANY(a%arr .NE. (/ (I, I = 1, 5) /)) ) STOP 21
          IF ( ANY(a%A0 .NE. (/ (I, I = 1, 10) /)) ) STOP 22
          IF ( ANY(a%A1 .NE. (/ (I, I = 1,  5) /)) ) STOP 23
          IF ( ANY(a%A2 .NE. (/ (I, I = 1, 15) /)) ) STOP 24

        CLASS DEFAULT
           STOP 25
      END SELECT

      IF ( c1%tag .NE. "Bohr" ) STOP 26
      IF ( ANY(c1%arr .NE. (/ (I, I = 1, 5) /)) ) STOP 27
      IF ( ANY(c1%A0 .NE. (/ (I, I = 1, 10) /)) ) STOP 28
      IF ( ANY(c1%A1 .NE. (/ (I, I = 1,  5) /)) ) STOP 29
      IF ( ANY(c1%A2 .NE. (/ (I, I = 1, 15) /)) ) STOP 30

      ALLOCATE ( ptr, SOURCE = Child(4,3,4,3) ("XLF", [101, 202, 303], [110, 220, 330], [111, 222, 333], &
                        [112, 223, 334, 121, 232, 343]) )
      SELECT TYPE ( a => foo(ptr) )
        CLASS IS (Child(4,*,4,*))
          call a%print
          IF ( a%tag .NE. "XLF" ) STOP 31
          IF ( ANY(a%arr .NE. [101, 202, 303]) ) STOP 32
          IF ( ANY(a%A0  .NE. [110, 220, 330]) ) STOP 33
          IF ( ANY(a%A1  .NE. [111, 222, 333]) ) STOP 34
          IF ( ANY(a%A2  .NE. [112, 223, 334, 121, 232, 343]) ) STOP 35

        CLASS DEFAULT
           STOP 36
      END SELECT

      contains

!      ASSOCIATE ( a => foo(b1) )
      subroutine ASSOCIATE1 ( a )
        class(base(4,*)), intent(in) :: a
        call a%print
        IF ( a%tag .NE. "Niels" ) STOP 10
        IF ( ANY(a%arr .NE. -1) ) STOP 11
      END subroutine

!      ASSOCIATE ( a => foo(b2) )
      subroutine ASSOCIATE2 ( a )
        class(base(4,*)), intent(in) :: a
        call a%print
        IF ( a%tag .NE. "Bohr" ) STOP 12
        IF ( ANY(a%arr .NE. (/ (I, I = 1, 5) /)) ) STOP 13
      END subroutine

!      ASSOCIATE ( a => foo(Base(4,6)( "Henrik", (/ (I**2, I = 1, 6) /) )) )
      subroutine ASSOCIATE3 ( a )
        class(base(4,*)), intent(in):: a
        call a%print
        IF ( a%tag .NE. "Henrik" ) STOP 14
        IF ( ANY(a%arr .NE. (/ (I**2, I = 1, 6) /)) ) STOP 15
      END subroutine

!      ASSOCIATE ( a => foo(ptr) )
      subroutine ASSOCIATE4 ( a )
        class(base(4,*)), intent(in) :: a
        call a%print
        IF ( a%tag .NE. "Schrodinger" ) STOP 16
        IF ( ANY(a%arr .NE. -99) ) STOP 17
      END subroutine

END PROGRAM FunctionResult13
