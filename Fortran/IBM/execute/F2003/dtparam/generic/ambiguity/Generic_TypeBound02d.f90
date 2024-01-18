!*  ===================================================================
!*
!*                               DTP - Generic Type-Bound
!*
!*  DATE                       : October 02, 2008
!*
!*  PRIMARY SUBROUTINES TESTED   : Generic Resolution - Derived-type parameters
!*  SECONDARY SUBROUTINES TESTED : Resolution for polymorphic objects
!*                                 based on the number of arguments
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

        CHARACTER(l1) :: tag

        CONTAINS
         PROCEDURE, PASS :: sub1
         GENERIC :: SUB =>  sub1
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (k2,l2)
        INTEGER, KIND :: k2
        INTEGER, LEN :: l2

        CONTAINS
         PROCEDURE, PASS :: sub2
         GENERIC :: SUB =>  sub1, sub2
      END TYPE Child

      TYPE, EXTENDS(Child) :: NextGen(k3,l3,l4)
        INTEGER, KIND :: k3
        INTEGER, LEN :: l3, l4

        CLASS(Base(k3,:)), ALLOCATABLE :: cmp
      END TYPE NextGen

      CONTAINS
!*
      SUBROUTINE sub1(Obj,poly)
      CLASS(Base(4,*)) :: Obj
      CLASS(Base(4,:)), ALLOCATABLE :: poly

      ALLOCATE (poly, source = Obj)
      IF ( .NOT. ALLOCATED(poly)) ERROR STOP 100

      poly%tag = 'sub1'

      END SUBROUTINE sub1

      SUBROUTINE sub2(Obj,Arg,poly)
      CLASS(Child(4,*,4,*)) :: Obj
      CLASS(Base(4,*)) :: Arg
      CLASS(Base(4,:)), ALLOCATABLE :: poly

      ALLOCATE (poly, source = Obj)
      IF ( .NOT. ALLOCATED(poly)) ERROR STOP 101

      poly%tag = 'sub2'

      END SUBROUTINE sub2

      END MODULE Mod1
!*
      PROGRAM Generic_TypeBound02d
      USE MOD1
      IMPLICIT NONE

      TYPE(Base(4,5))  :: base1
      TYPE(child(4,10,4,5)) :: child1
      TYPE(child(4,10,4,10)), TARGET :: tgt1
      TYPE(NextGen(4,10,4,10,4,1,1)) :: dtv

      CLASS(Base(4,:)), ALLOCATABLE :: poly1
      CLASS(Base(4,:)), POINTER :: pntr

      call base1%SUB(poly1)                ! 2 arguments call sub1
      IF (poly1%tag .NE. 'sub1') ERROR STOP 10
      DEALLOCATE(poly1)

      call child1%SUB(poly1)               ! 2 arguments call sub1
      IF (poly1%tag .NE. 'sub1') ERROR STOP 11
      DEALLOCATE(poly1)

      call tgt1%SUB(poly1)                 ! 2 arguments call sub1
      IF (poly1%tag .NE. 'sub1') ERROR STOP 12
      DEALLOCATE(poly1)

      call dtv%SUB(poly1)                  ! 2 arguments call sub1
      IF (poly1%tag .NE. 'sub1') ERROR STOP 13
      DEALLOCATE(poly1)

      pntr => tgt1
      call pntr%SUB(poly1)                 ! 2 arguments call sub1
      IF (poly1%tag .NE. 'sub1') ERROR STOP 14
      DEALLOCATE(poly1)

      call tgt1%SUB(pntr,poly1)            ! 3 arguments call sub2
      IF (poly1%tag .NE. 'sub2') ERROR STOP 15
      DEALLOCATE(poly1)

      call tgt1%SUB(tgt1,poly1)            ! 3 arguments call sub2
      IF (poly1%tag .NE. 'sub2') ERROR STOP 16
      DEALLOCATE(poly1)

      call tgt1%SUB(dtv,poly1)             ! 3 arguments call sub2
      IF (poly1%tag .NE. 'sub2') ERROR STOP 17
      DEALLOCATE(poly1)

      call dtv%SUB(tgt1,poly1)             ! 3 arguments call sub2
      IF (poly1%tag .NE. 'sub2') ERROR STOP 18
      DEALLOCATE(poly1)

      call dtv%SUB(dtv,poly1)              ! 3 arguments call sub2
      IF (poly1%tag .NE. 'sub2') ERROR STOP 19

      call tgt1%SUB(poly1,dtv%cmp)         ! 3 arguments call sub2
      IF (dtv%cmp%tag .NE. 'sub2') ERROR STOP 20
      DEALLOCATE(dtv%cmp)

      call tgt1%SUB(tgt1,dtv%cmp)          ! 3 arguments call sub2
      IF (dtv%cmp%tag .NE. 'sub2') ERROR STOP 21
      DEALLOCATE(dtv%cmp)

      call tgt1%SUB(pntr,dtv%cmp)          ! 3 arguments call sub2
      IF (dtv%cmp%tag .NE. 'sub2') ERROR STOP 22
      DEALLOCATE(dtv%cmp)

      call tgt1%SUB(dtv,dtv%cmp)           ! 3 arguments call sub2
      IF (dtv%cmp%tag .NE. 'sub2') ERROR STOP 23
      DEALLOCATE(poly1)

      call dtv%cmp%SUB(poly1)              ! 2 arguments call sub1
      IF (poly1%tag .NE. 'sub1') ERROR STOP 24
      DEALLOCATE(poly1)

      call tgt1%SUB(dtv%cmp,poly1)         ! 3 arguments call sub2
      IF (poly1%tag .NE. 'sub2') ERROR STOP 25
      DEALLOCATE(poly1)
      DEALLOCATE(dtv%cmp)

     END PROGRAM Generic_TypeBound02d
