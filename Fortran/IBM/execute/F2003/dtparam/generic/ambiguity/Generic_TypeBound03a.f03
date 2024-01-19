!*  ===================================================================
!*
!*                               DTP - Generic Type-Bound
!*
!*  DATE                       : October 02, 2008
!*
!*  PRIMARY SUBROUTINES TESTED   : Generic Resolution - Derived-type parameters
!*  SECONDARY SUBROUTINES TESTED : Resolution for polymorphic objects
!*                                 based on the number of arguments using PASS
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
!*                                    & [ [, binding-attr -list ] :: ] &
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

        CONTAINS
         PROCEDURE, PASS :: sub1
         PROCEDURE, PASS :: sub2
         GENERIC :: SUB =>  sub2, sub1
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (k2)
        INTEGER, KIND :: k2
      END TYPE Child

      TYPE, EXTENDS(Child) :: NextGen(k3)
        INTEGER, KIND :: k3

        CONTAINS
         PROCEDURE, PASS :: sub3
         GENERIC :: SUB =>  sub3
      END TYPE NextGen

      CHARACTER(10) :: tag

      CONTAINS
!*
      SUBROUTINE sub1(Obj)
      CLASS(Base(4,*)) :: Obj
      CLASS(Base(4,:)), POINTER  :: pntr

      ALLOCATE (pntr, source = Obj)
      IF ( .NOT. ASSOCIATED(pntr)) ERROR STOP 30

      tag = '1'

      END SUBROUTINE sub1

      SUBROUTINE sub2(Obj,Arg)
      CLASS(Base(4,*)) :: Obj, Arg
      CLASS(Base(4,:)), POINTER  :: pntr

      ALLOCATE (pntr, source = Obj)
      IF ( .NOT. ASSOCIATED(pntr)) ERROR STOP 31

      tag = '2'

      END SUBROUTINE sub2

      SUBROUTINE sub3(Obj,Arg1,Arg2)
      CLASS(NextGen(4,*,4,4)) :: Obj
      CLASS(Base(4,*)) :: Arg1, Arg2
      CLASS(NextGen(4,:,4,4)), POINTER :: pntr

      ALLOCATE (pntr, source = Obj)
      IF ( .NOT. ASSOCIATED(pntr)) ERROR STOP 31

      tag = '3'

      END SUBROUTINE sub3

      END MODULE Mod1
!*
      PROGRAM Generic_TypeBound03a
      USE MOD1
      IMPLICIT NONE

      CLASS(Base(4,:)), POINTER :: poly1
      TYPE(Child(4,5,4)), TARGET :: tgt1
      TYPE(Base(4,5))  :: base1
      TYPE(NextGen(4,10,4,4)) :: dtv

      CALL base1%SUB()
      IF ( tag .NE. '1' ) ERROR STOP 10

      ALLOCATE(Base(4,10):: poly1)
      CALL poly1%SUB()
      IF ( tag .NE. '1' ) ERROR STOP 11

      poly1 => tgt1
      CALL poly1%SUB()
      IF ( tag .NE. '1' ) ERROR STOP 12

      ALLOCATE(NextGen(4,10,4,4):: poly1)
      CALL poly1%SUB()
      IF ( tag .NE. '1' ) ERROR STOP 13

      CALL poly1%SUB(base1)
      IF ( tag .NE. '2' ) ERROR STOP 14
      CALL poly1%SUB(tgt1)
      IF ( tag .NE. '2' ) ERROR STOP 15
      CALL poly1%SUB(poly1)
      IF ( tag .NE. '2' ) ERROR STOP 16

      SELECT TYPE (poly1) ! call possible only within select type
          CLASS IS (NextGen(4,*,4,4))
            CALL poly1%SUB(base1,poly1)
            IF ( tag .NE. '3' ) ERROR STOP 17
            CALL poly1%SUB(tgt1,base1)
            IF ( tag .NE. '3' ) ERROR STOP 18
            CALL poly1%SUB(poly1,poly1)
            IF ( tag .NE. '3' ) ERROR STOP 19

          CLASS DEFAULT
           STOP 32
      END SELECT

      ALLOCATE(NextGen(4,10,8,8):: poly1)
      CALL poly1%SUB()
      IF ( tag .NE. '1' ) ERROR STOP 20

      CALL dtv%SUB(base1)
      IF ( tag .NE. '2' ) ERROR STOP 21
      CALL dtv%SUB(tgt1)
      IF ( tag .NE. '2' ) ERROR STOP 22
      CALL dtv%SUB(poly1)
      IF ( tag .NE. '2' ) ERROR STOP 23
      CALL dtv%SUB(dtv)
      IF ( tag .NE. '2' ) ERROR STOP 24

      CALL dtv%SUB(base1,dtv)
      IF ( tag .NE. '3' ) ERROR STOP 25
      CALL dtv%SUB(tgt1,tgt1)
      IF ( tag .NE. '3' ) ERROR STOP 26
      CALL dtv%SUB(poly1,poly1)
      IF ( tag .NE. '3' ) ERROR STOP 27
      CALL dtv%SUB(dtv,dtv)
      IF ( tag .NE. '3' ) ERROR STOP 28

      END PROGRAM Generic_TypeBound03a
