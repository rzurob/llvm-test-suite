!*  ===================================================================
!*
!*  DATE                       : June 2, 2015
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
!*  TEST CASE ADAPTED FROM     : $(tsrcdir)/F2003/dtparam/allocate/SourceExp/AllocateWithSourceExp06.f
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

      TYPE(Base(4,:)),  POINTER :: b1, c1
      CLASS(Base(4,:)), ALLOCATABLE :: b_poly, c_poly

      ALLOCATE(b1, c1, SOURCE=Base(4,5)('Base', -99))
      CALL alloc_auto(b1,c1)
      DEALLOCATE(b1); DEALLOCATE(c1)

      ALLOCATE(b_poly, c_poly, SOURCE=Base(4,10)('Base', -99))
      CALL alloc_auto(b_poly,c_poly)
      DEALLOCATE(b_poly); DEALLOCATE(c_poly)

      ALLOCATE(b1, c1, SOURCE=Base(4,3)('Base', -99))

      ALLOCATE(b_poly, c_poly, SOURCE=Child(4,2,4,3)('Child', 22, b1 , b1))
      CALL alloc_auto(b_poly,c_poly)
      DEALLOCATE(b_poly); DEALLOCATE(c_poly)

      CONTAINS

      SUBROUTINE Alloc_auto(Arg1, Arg2)
      CLASS(*), INTENT(IN) :: Arg1, Arg2
      CLASS(*), ALLOCATABLE :: Obj1, Obj2

          SELECT TYPE ( Arg1 )
              CLASS IS (Base(4,*))
                  ALLOCATE(Obj1, Obj2, SOURCE=Arg1)

                  SELECT TYPE ( Obj1 )
                      CLASS IS (Base(4,*))
                        IF (ANY(Obj1%my_arr .NE. -99)) ERROR STOP 10
                        IF (ANY(Obj1%my_arr .NE. Arg1%my_arr)) ERROR STOP 11
                        IF (Obj1%name .NE. 'Base') ERROR STOP 12
                        IF (Obj1%name .NE. Arg1%name) ERROR STOP 13

                      CLASS DEFAULT
                         STOP 14
                  END SELECT
                  SELECT TYPE ( Obj2 )
                      CLASS IS (Base(4,*))
                        IF (ANY(Obj2%my_arr .NE. -99)) ERROR STOP 110
                        IF (ANY(Obj2%my_arr .NE. Arg1%my_arr)) ERROR STOP 111
                        IF (Obj2%name .NE. 'Base') ERROR STOP 112
                        IF (Obj2%name .NE. Arg1%name) ERROR STOP 113

                      CLASS DEFAULT
                         STOP 114
                  END SELECT

              CLASS IS (Child(4,*,4,*))
                  ALLOCATE(Obj1, Obj2, SOURCE=Arg1)

                  SELECT TYPE ( Obj1 )
                     CLASS IS (Child(4,*,4,*))
                        IF (ANY(Obj1%my_arr .NE. 22)) ERROR STOP 15
                        IF (ANY(Obj1%my_arr .NE. Arg1%my_arr)) ERROR STOP 16
                        IF (Obj1%name .NE. 'Ch') ERROR STOP 17
                        IF (Obj1%name .NE. Arg1%name) ERROR STOP 18

                        IF (ANY(Obj1%b_cmp%my_arr .NE. -99)) ERROR STOP 19
                        IF (ANY(Obj1%b_cmp%my_arr .NE. Arg1%b_cmp%my_arr)) ERROR STOP 20
                        IF (Obj1%b_cmp%name .NE. 'Bas') ERROR STOP 21
                        IF (Obj1%b_cmp%name .NE. Arg1%b_cmp%name) ERROR STOP 22

                        IF (ANY(Obj1%c_cmp%my_arr .NE. -99)) ERROR STOP 23
                        IF (ANY(Obj1%c_cmp%my_arr .NE. Arg1%c_cmp%my_arr)) ERROR STOP 24
                        IF (Obj1%c_cmp%name .NE. 'Bas') ERROR STOP 25
                        IF (Obj1%c_cmp%name .NE. Arg1%c_cmp%name) ERROR STOP 26

                      CLASS DEFAULT
                         STOP 27
                  END SELECT
                  SELECT TYPE ( Obj1 )
                     CLASS IS (Child(4,*,4,*))
                        IF (ANY(Obj1%my_arr .NE. 22)) ERROR STOP 115
                        IF (ANY(Obj1%my_arr .NE. Arg1%my_arr)) ERROR STOP 116
                        IF (Obj1%name .NE. 'Ch') ERROR STOP 117
                        IF (Obj1%name .NE. Arg1%name) ERROR STOP 118

                        IF (ANY(Obj1%b_cmp%my_arr .NE. -99)) ERROR STOP 119
                        IF (ANY(Obj1%b_cmp%my_arr .NE. Arg1%b_cmp%my_arr)) ERROR STOP 120
                        IF (Obj1%b_cmp%name .NE. 'Bas') ERROR STOP 121
                        IF (Obj1%b_cmp%name .NE. Arg1%b_cmp%name) ERROR STOP 122

                        IF (ANY(Obj1%c_cmp%my_arr .NE. -99)) ERROR STOP 123
                        IF (ANY(Obj1%c_cmp%my_arr .NE. Arg1%c_cmp%my_arr)) ERROR STOP 124
                        IF (Obj1%c_cmp%name .NE. 'Bas') ERROR STOP 125
                        IF (Obj1%c_cmp%name .NE. Arg1%c_cmp%name) ERROR STOP 126

                      CLASS DEFAULT
                         STOP 127
                  END SELECT

              CLASS DEFAULT
                 STOP 324
          END SELECT

          DEALLOCATE(Obj1, Obj2)

          SELECT TYPE ( Arg2 )
              CLASS IS (Base(4,*))
                  ALLOCATE(Obj1, Obj2, SOURCE=Arg2)

                  SELECT TYPE ( Obj1 )
                      CLASS IS (Base(4,*))
                        IF (ANY(Obj1%my_arr .NE. -99)) ERROR STOP 210
                        IF (ANY(Obj1%my_arr .NE. Arg2%my_arr)) ERROR STOP 211
                        IF (Obj1%name .NE. 'Base') ERROR STOP 212
                        IF (Obj1%name .NE. Arg2%name) ERROR STOP 213

                      CLASS DEFAULT
                         STOP 214
                  END SELECT
                  SELECT TYPE ( Obj2 )
                      CLASS IS (Base(4,*))
                        IF (ANY(Obj2%my_arr .NE. -99)) ERROR STOP 2110
                        IF (ANY(Obj2%my_arr .NE. Arg2%my_arr)) ERROR STOP 2111
                        IF (Obj2%name .NE. 'Base') ERROR STOP 2112
                        IF (Obj2%name .NE. Arg2%name) ERROR STOP 2113

                      CLASS DEFAULT
                         STOP 2114
                  END SELECT

              CLASS IS (Child(4,*,4,*))
                  ALLOCATE(Obj1, Obj2, SOURCE=Arg2)

                  SELECT TYPE ( Obj1 )
                     CLASS IS (Child(4,*,4,*))
                        IF (ANY(Obj1%my_arr .NE. 22)) ERROR STOP 215
                        IF (ANY(Obj1%my_arr .NE. Arg2%my_arr)) ERROR STOP 216
                        IF (Obj1%name .NE. 'Ch') ERROR STOP 217
                        IF (Obj1%name .NE. Arg2%name) ERROR STOP 218

                        IF (ANY(Obj1%b_cmp%my_arr .NE. -99)) ERROR STOP 219
                        IF (ANY(Obj1%b_cmp%my_arr .NE. Arg2%b_cmp%my_arr)) ERROR STOP 220
                        IF (Obj1%b_cmp%name .NE. 'Bas') ERROR STOP 221
                        IF (Obj1%b_cmp%name .NE. Arg2%b_cmp%name) ERROR STOP 222

                        IF (ANY(Obj1%c_cmp%my_arr .NE. -99)) ERROR STOP 223
                        IF (ANY(Obj1%c_cmp%my_arr .NE. Arg2%c_cmp%my_arr)) ERROR STOP 224
                        IF (Obj1%c_cmp%name .NE. 'Bas') ERROR STOP 225
                        IF (Obj1%c_cmp%name .NE. Arg2%c_cmp%name) ERROR STOP 226

                      CLASS DEFAULT
                         STOP 227
                  END SELECT
                  SELECT TYPE ( Obj1 )
                     CLASS IS (Child(4,*,4,*))
                        IF (ANY(Obj1%my_arr .NE. 22)) ERROR STOP 2115
                        IF (ANY(Obj1%my_arr .NE. Arg2%my_arr)) ERROR STOP 2116
                        IF (Obj1%name .NE. 'Ch') ERROR STOP 2117
                        IF (Obj1%name .NE. Arg2%name) ERROR STOP 2118

                        IF (ANY(Obj1%b_cmp%my_arr .NE. -99)) ERROR STOP 2119
                        IF (ANY(Obj1%b_cmp%my_arr .NE. Arg2%b_cmp%my_arr)) ERROR STOP 2120
                        IF (Obj1%b_cmp%name .NE. 'Bas') ERROR STOP 2121
                        IF (Obj1%b_cmp%name .NE. Arg2%b_cmp%name) ERROR STOP 2122

                        IF (ANY(Obj1%c_cmp%my_arr .NE. -99)) ERROR STOP 2123
                        IF (ANY(Obj1%c_cmp%my_arr .NE. Arg2%c_cmp%my_arr)) ERROR STOP 2124
                        IF (Obj1%c_cmp%name .NE. 'Bas') ERROR STOP 2125
                        IF (Obj1%c_cmp%name .NE. Arg2%c_cmp%name) ERROR STOP 2126

                      CLASS DEFAULT
                         STOP 2127
                  END SELECT

              CLASS DEFAULT
                 STOP 324
          END SELECT

        DEALLOCATE( Obj1, Obj2 )

        END SUBROUTINE Alloc_auto

END PROGRAM AllocateWithSourceExp06
