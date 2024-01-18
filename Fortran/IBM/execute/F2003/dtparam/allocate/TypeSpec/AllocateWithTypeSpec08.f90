!*  ===================================================================
!*
!*  DATE                       : January 20, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : ALLOCATE Statement with type-spec
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : Deferred LEN parameter
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!* allocate-stmt is
!*   ALLOCATE ( [ type-spec :: ] allocation-list [, alloc-opt-list ] )
!*
!* Defect 351490
!*
!234567890123456789012345678901234567890123456789012345678901234567890
MODULE Mod
      IMPLICIT NONE

      TYPE Base  (k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN  :: l1

        CHARACTER(2*l1+k1) :: status
        CHARACTER(l1) :: tag
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (k2,l2)
        INTEGER, KIND :: k2
        INTEGER, LEN  :: l2

        CLASS(Base(k2,l2-l1)), ALLOCATABLE :: b_cmp
      END TYPE Child
END MODULE Mod
PROGRAM AllocateWithTypeSpec07
      USE Mod
      IMPLICIT NONE

      CLASS(Base(4,10)), ALLOCATABLE :: b1

      IF ( ALLOCATED(b1)) STOP 10

      ALLOCATE(Child(4,10,4,20) :: b1)
      CALL verify_type_param(b1)
      IF (b1%tag .NE. 'Child') STOP 11

      CALL alloc_comp(b1)
      IF (b1%status .NE. '1 allocation done') STOP 12

      SELECT TYPE ( b1 )
        CLASS IS (Child(4,*,4,*))
            IF (.NOT. ALLOCATED(b1%b_cmp)) STOP 13
            CALL verify_type_param(b1%b_cmp)
            IF (b1%b_cmp%tag .NE. 'Base') STOP 14

        CLASS DEFAULT
            STOP 15
      END SELECT

      CALL duplicate(b1)

      SELECT TYPE ( b1 )
        CLASS IS (Child(4,*,4,*))
           DEALLOCATE(b1%b_cmp)

        CLASS DEFAULT
            STOP 16
      END SELECT

      DEALLOCATE(b1)

      CONTAINS
!*
      SUBROUTINE verify_type_param(Arg)
         CLASS(Base(4,*)) :: Arg

         SELECT TYPE ( Arg )
              CLASS IS (Base(4,*))
                 Arg%tag = 'Base'
                 IF (Arg%l1 .NE. 10) STOP 20

              CLASS IS (Child(4,*,4,*))
                 Arg%tag = 'Child'
                 IF (Arg%l1 .NE. 10) STOP 21
                 IF (Arg%l2 .NE. 20) STOP 22

              CLASS DEFAULT
                 STOP 23
         END SELECT

      END SUBROUTINE verify_type_param

      SUBROUTINE alloc_comp(Arg)
         CLASS(Base(4,*)), ALLOCATABLE :: Arg

         SELECT TYPE ( Arg )
              CLASS IS (Child(4,*,4,*))
                 IF ( ALLOCATED(Arg%b_cmp)) STOP 30
                 ALLOCATE(Base(4,Arg%l2-Arg%l1) :: Arg%b_cmp)
                 Arg%status = '1 allocation done'

              CLASS DEFAULT
                 STOP 31
         END SELECT

      END SUBROUTINE alloc_comp

      SUBROUTINE duplicate(Arg)
         CLASS(Base(4,*)), ALLOCATABLE :: Arg
         CLASS(Base(4,:)), ALLOCATABLE :: AutoObj

         ALLOCATE(AutoObj, source = Arg)
         IF (AutoObj%status .NE. '1 allocation done') STOP 40

         SELECT TYPE ( Arg )
           CLASS IS (Child(4,*,4,*))
              IF (.NOT. ALLOCATED(Arg%b_cmp)) STOP 41

           CLASS DEFAULT
              STOP 42
         END SELECT

         AutoObj%tag = 'NULL'
         CALL verify_type_param(AutoObj)
         IF (AutoObj%tag .NE. 'Child') STOP 43

         DEALLOCATE(AutoObj)

      END SUBROUTINE duplicate

END PROGRAM AllocateWithTypeSpec07
