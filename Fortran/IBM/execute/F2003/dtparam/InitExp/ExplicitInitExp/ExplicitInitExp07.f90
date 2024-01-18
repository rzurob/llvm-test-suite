!*  ===================================================================
!*
!*  DATE                       : May 24, 2009
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Explicit Init Expression
!*  SECONDARY FUNCTIONS TESTED : Assumed LEN parameter
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
      IMPLICIT NONE

       TYPE Base (k1,l1)
         INTEGER, KIND :: k1
         INTEGER, LEN  :: l1

         INTEGER(k1) :: A1(l1)
         CHARACTER(l1) :: C1
       END TYPE

       TYPE, EXTENDS(Base) :: Child (k2,l2)
         INTEGER, KIND :: k2
         INTEGER, LEN  :: l2

         INTEGER(k2) :: A2(l2)
         CHARACTER(l2) :: C2
         TYPE(Base(k2,l2)) :: bcomp
      END TYPE

      CONTAINS

      SUBROUTINE CheckBase (arg)
        TYPE(Base(4,*)) :: arg
        INTEGER :: I

        IF ( ANY(arg%A1 .NE. [(I, I = 1, 10)]) ) STOP 10
        IF ( arg%C1 .NE. 'ABCDE' ) STOP 11

      END SUBROUTINE

      SUBROUTINE CheckChild (arg)
        TYPE(Child(4,*,4,*)) :: arg
        INTEGER :: I

        IF ( ANY(arg%A1 .NE. [(2*I, I = 1, 10)]) ) STOP 12
        IF ( arg%C1 .NE. 'IBM' ) STOP 13
        IF ( ANY(arg%A2 .NE. [(3*I, I = 1, 10)]) ) STOP 14
        IF ( arg%C2 .NE. 'XLFtest' ) STOP 15

        call CheckBase(arg%bcomp)

      END SUBROUTINE
END MODULE
PROGRAM ExplicitInitExp07
      USE Mod
      IMPLICIT NONE

      INTEGER :: I
      INTEGER, PARAMETER :: Iconst(10) = [(I, I = 1, 10)]

      TYPE(Base(4,10)) :: b1 = Base(4,10) ( Iconst, 'ABCDE' )
      TYPE(Child(4,10,4,10)) :: c1 = Child(4,10,4,10) ( 2*Iconst, 'IBM', 3*Iconst, 'XLFtest', Base(4,10) (Iconst, 'ABCDE') )

      CLASS(Base(4,:)), ALLOCATABLE :: poly

      call CheckBase(b1)

      call CheckChild(c1)

      ALLOCATE( poly, SOURCE = b1 )
      call CheckBase(poly)
      DEALLOCATE( poly )

      ALLOCATE( poly, SOURCE = c1 )
      SELECT TYPE ( poly )
          CLASS IS (Child(4,*,4,*))
             call CheckChild(poly)

          CLASS DEFAULT
            STOP 20
      END SELECT
      DEALLOCATE( poly )

END PROGRAM ExplicitInitExp07
