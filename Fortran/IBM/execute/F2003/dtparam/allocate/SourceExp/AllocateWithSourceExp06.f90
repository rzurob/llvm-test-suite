!*  ===================================================================
!*
!*  DATE                       : January 20, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : ALLOCATE Statement with source expression
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
!* allocate-stmt is
!*   ALLOCATE ( [ type-spec :: ] allocation-list [, alloc-opt-list ] )
!*
!*  Defect 361745
!*
!234567890123456789012345678901234567890123456789012345678901234567890
PROGRAM AllocateWithSourceExp06
      IMPLICIT NONE

      TYPE Base  (k1,l1)
        INTEGER, KIND :: k1 = KIND(0)
        INTEGER, LEN  :: l1 = 1

        CHARACTER(l1)  :: name
        INTEGER(k1) :: my_arr(l1)
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (k2,l2)
        INTEGER, KIND :: k2 = KIND(0)
        INTEGER, LEN  :: l2 = 2

        CLASS(Base(k2,l2)), POINTER :: b_cmp
        CLASS(Base(k2,l2)), POINTER :: c_cmp
      END TYPE Child

      TYPE(Base(4,:)), POINTER :: b1
      CLASS(Base(4,:)), ALLOCATABLE :: b_poly

      ALLOCATE(b1, SOURCE=Base(4,5)('Base', -99))
      CALL alloc_auto(b1)
      DEALLOCATE(b1)

      ALLOCATE(b_poly, SOURCE=Base(4,10)('Base', -99))
      CALL alloc_auto(b_poly)
      DEALLOCATE(b_poly)

      ALLOCATE(b1, SOURCE=Base(4,3)('Base', -99))
      ALLOCATE(b_poly, SOURCE=Child(4,2,4,3)('Child', 22, b1 , b1))
      CALL alloc_auto(b_poly)
      DEALLOCATE(b_poly)

      CONTAINS

      SUBROUTINE Alloc_auto(Arg)
      CLASS(*), INTENT(IN) :: Arg
      CLASS(*), ALLOCATABLE :: Obj

          SELECT TYPE ( Arg )
              CLASS IS (Base(4,*))
                  ALLOCATE(Obj, SOURCE=Arg)

                  SELECT TYPE ( Obj )
                      CLASS IS (Base(4,*))
                        IF (ANY(Obj%my_arr .NE. -99)) STOP 10
                        IF (ANY(Obj%my_arr .NE. Arg%my_arr)) STOP 11
                        IF (Obj%name .NE. 'Base') STOP 12
                        IF (Obj%name .NE. Arg%name) STOP 13

                      CLASS DEFAULT
                         STOP 14
                  END SELECT

              CLASS IS (Child(4,*,4,*))
                  ALLOCATE(Obj, SOURCE=Arg)

                  SELECT TYPE ( Obj )
                     CLASS IS (Child(4,*,4,*))
                        IF (ANY(Obj%my_arr .NE. 22)) STOP 15
                        IF (ANY(Obj%my_arr .NE. Arg%my_arr)) STOP 16
                        IF (Obj%name .NE. 'Ch') STOP 17
                        IF (Obj%name .NE. Arg%name) STOP 18

                        IF (ANY(Obj%b_cmp%my_arr .NE. -99)) STOP 19
                        IF (ANY(Obj%b_cmp%my_arr .NE. Arg%b_cmp%my_arr)) STOP 20
                        IF (Obj%b_cmp%name .NE. 'Bas') STOP 21
                        IF (Obj%b_cmp%name .NE. Arg%b_cmp%name) STOP 22

                        IF (ANY(Obj%c_cmp%my_arr .NE. -99)) STOP 23
                        IF (ANY(Obj%c_cmp%my_arr .NE. Arg%c_cmp%my_arr)) STOP 24
                        IF (Obj%c_cmp%name .NE. 'Bas') STOP 25
                        IF (Obj%c_cmp%name .NE. Arg%c_cmp%name) STOP 26

                      CLASS DEFAULT
                         STOP 27
                  END SELECT

              CLASS DEFAULT
                 STOP 24
          END SELECT

        DEALLOCATE( Obj )

        END SUBROUTINE Alloc_auto

END PROGRAM AllocateWithSourceExp06
