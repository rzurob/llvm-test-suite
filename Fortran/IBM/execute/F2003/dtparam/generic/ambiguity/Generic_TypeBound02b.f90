!*  ===================================================================
!*
!*                               DTP - Generic Type-Bound
!*
!*  DATE                       : October 02, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY SUBTIONS TESTED   : Generic Resolution - Derived-type parameters
!*  SECONDARY SUBTIONS TESTED : Resolution based on KIND type parameter
!*                               polymorphic objects
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : GENERIC
!*
!*  DESCRIPTION                :
!*
!*  R448 type-bound-procedure-part is contains-stmt
!*                                     [ binding-private-stmt ]
!*                                     proc-binding-stmt
!*                                     [ proc-binding-stmt ] ...
!*
!*  R450 proc-binding-stmt is specific-binding
!*                         or generic-binding
!*                         or final-binding
!*
!*  R451 specific-binding is PROCEDURE [ (interface-name) ] &
!*                                    & [ [ , binding-attr -list ] :: ] &
!*                                    & binding-name [ => procedure-name ]
!*
!*  R452 generic-binding is GENERIC [, access-spec ] :: generic-spec => binding-name-list
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      MODULE Mod1
      IMPLICIT NONE

      TYPE Base  (k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN :: l1

        INTEGER(k1) :: sum

        CONTAINS
         PROCEDURE, PASS :: SUB_BASE1
         PROCEDURE, PASS :: SUB_BASE2
         GENERIC :: SUB =>  SUB_BASE1, SUB_BASE2
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (k2)
        INTEGER, KIND :: k2
      END TYPE Child

      TYPE, EXTENDS(Child) :: NextGen(k3)
        INTEGER, KIND :: k3
      END TYPE NextGen

      CONTAINS
!*
      SUBROUTINE SUB_BASE1(Obj)
        CLASS(Base(4,*)), INTENT(INOUT) :: Obj

        SELECT TYPE (Obj)
          TYPE IS (NextGen(4,*,4,4))
             Obj%sum = Obj%k1 + Obj%k2 + Obj%k3

          TYPE IS (NextGen(4,*,4,8))
             Obj%sum = Obj%k1 + Obj%k2 + Obj%k3

          TYPE IS (NextGen(4,*,8,4))
             Obj%sum = Obj%k1 + Obj%k2 + Obj%k3

          TYPE IS (NextGen(4,*,8,8))
             Obj%sum = Obj%k1 + Obj%k2 + Obj%k3

          TYPE IS (Child(4,*,4))
             Obj%sum = Obj%k1 + Obj%k2

          TYPE IS (Child(4,*,8))
             Obj%sum = Obj%k1 + Obj%k2

          TYPE IS (Base(4,*))
             Obj%sum = Obj%k1

          CLASS DEFAULT
           STOP 110
      END SELECT

      END SUBROUTINE

      SUBROUTINE SUB_BASE2(Obj)
        CLASS(Base(8,*)), INTENT(INOUT) :: Obj

        SELECT TYPE (Obj)
          TYPE IS (NextGen(8,*,4,4))
             Obj%sum = Obj%k1 + Obj%k2 + Obj%k3

          TYPE IS (NextGen(8,*,4,8))
             Obj%sum = Obj%k1 + Obj%k2 + Obj%k3

          TYPE IS (NextGen(8,*,8,4))
             Obj%sum = Obj%k1 + Obj%k2 + Obj%k3

          TYPE IS (NextGen(8,*,8,8))
             Obj%sum = Obj%k1 + Obj%k2 + Obj%k3

          TYPE IS (Child(8,*,4))
             Obj%sum = Obj%k1 + Obj%k2

          TYPE IS (Child(8,*,8))
             Obj%sum = Obj%k1 + Obj%k2

          TYPE IS (Base(8,*))
             Obj%sum = Obj%k1

          CLASS DEFAULT
           STOP 111
      END SELECT

      END SUBROUTINE

      END MODULE Mod1
!*
      PROGRAM Generic_TypeBound02b
      USE MOD1
      IMPLICIT NONE

      TYPE(Child(4,5,4)), TARGET :: tgt1
      TYPE(Base(4,5))  :: base1
      TYPE(Base(8,5))  :: base2

      CLASS(Base(4,:)), POINTER :: poly1
      CLASS(Child(8,:,4)), POINTER :: poly2

      CALL base1%SUB ()
      IF( base1%sum .NE. 4) STOP 10

      CALL base2%SUB ()
      IF( base2%sum .NE. 8) STOP 11

      ALLOCATE(Base(4,10):: poly1)
      IF ( .NOT. ASSOCIATED(poly1)) STOP 12

      CALL poly1%SUB()
      IF( poly1%sum .NE. 4) STOP 13

      poly1 => tgt1
      IF ( .NOT. ASSOCIATED(poly1)) STOP 14

      CALL poly1%SUB()
      IF( poly1%sum .NE. 8) STOP 15

      ALLOCATE(NextGen(4,10,8,8):: poly1)
      IF ( .NOT. ASSOCIATED(poly1)) STOP 16

      CALL poly1%SUB()
      IF( poly1%sum .NE. 20) STOP 17

      CALL SUB1(poly1)

      CALL SUB2(poly2)

      CONTAINS

      SUBROUTINE Sub1 (Arg)
      CLASS(Base(4,:)), POINTER ::  Arg

      ALLOCATE(Base(4,10):: Arg)
      IF ( .NOT. ASSOCIATED(Arg)) STOP 20

      CALL Arg%SUB()
      IF( Arg%sum .NE. 4) STOP 21

      Arg => tgt1
      IF ( .NOT. ASSOCIATED(Arg)) STOP 22

      CALL Arg%SUB()
      IF( Arg%sum .NE. 8) STOP 23

      ALLOCATE(NextGen(4,10,8,8):: Arg)
      IF ( .NOT. ASSOCIATED(Arg)) STOP 24

      CALL Arg%SUB()
      IF( Arg%sum .NE. 20) STOP 25

      END SUBROUTINE SUB1

      SUBROUTINE Sub2 (Arg)
      CLASS(Child(8,:,4)), POINTER :: Arg
      TYPE(Child(8,20,4)), TARGET :: tgt2

      Arg => tgt2
      IF ( .NOT. ASSOCIATED(Arg)) STOP 17

      CALL Arg%SUB()
      IF( Arg%sum .NE. 12) STOP 18

      END SUBROUTINE SUB2

      END PROGRAM Generic_TypeBound02b
