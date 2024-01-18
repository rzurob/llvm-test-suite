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
!*  Defect 356545
!*
!234567890123456789012345678901234567890123456789012345678901234567890
MODULE Mod
      TYPE :: Base(k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN :: l1

        CHARACTER(LEN=l1) :: tag = "Niels"
        INTEGER(k1) :: arr(l1) = -1
      END TYPE

      TYPE,  EXTENDS(Base) :: Child (k2,l2)
        INTEGER, KIND :: k2
        INTEGER, LEN  :: l2

        INTEGER(k2) :: A0(l2), A1(l1), A2(l1+l2)
      END TYPE

      CONTAINS

      FUNCTION foo(Arg)
        CLASS(Base(4,*)), INTENT(IN) :: Arg
        TYPE(Base(4,:)), ALLOCATABLE :: foo
          foo = Arg
      END FUNCTION
END MODULE
PROGRAM FunctionResult12
      USE Mod
      IMPLICIT NONE
      INTEGER :: I
      TYPE(Base(4,5)) :: b1, b2 = Base(4,5)( "Bohr", (/ (I, I = 1, 5) /) )
      TYPE(Child(4,5,4,10)) :: c1
      CLASS(Base(4,:)), POINTER :: ptr

      print *, foo(b1)

      print *, foo(b2)

      call ASSOCIATE_replacer (foo(Base(4,6)( "Henrik", (/(I**2, I = 1, 6) /))) )

      ALLOCATE ( ptr, SOURCE = Base(4,11) ("Schrodinger", -99) )
      print *, foo(ptr)

      print *, foo(c1)

      c1%Base = b2 ; c1%A0 = (/ (I, I = 1, 10) /); c1%A1 = (/ (I, I = 1, 5) /); c1%A2 = (/ (I, I = 1, 15) /)
      print *, foo(c1)
      IF ( ANY(c1%A0 .NE. (/ (I, I = 1, 10) /)) ) STOP 10
      IF ( ANY(c1%A1 .NE. (/ (I, I = 1,  5) /)) ) STOP 11
      IF ( ANY(c1%A2 .NE. (/ (I, I = 1, 15) /)) ) STOP 12
      print *, c1

      contains

!      ASSOCIATE ( a => foo(Base(4,6)( "Henrik", (/ (I**2, I = 1, 6) /) )) )
      subroutine ASSOCIATE_replacer ( a )
        type(base(4,*)), intent(in) :: a
        print *, a
      END subroutine
END PROGRAM FunctionResult12
