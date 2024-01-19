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
      IMPLICIT NONE

      TYPE Base (k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN  :: l1

        INTEGER(k1) :: A0(l1)
        CHARACTER(l1) :: name
        INTEGER(k1) :: A1(l1)
      END TYPE

      TYPE,  EXTENDS(Base) :: Child (k2,l2)
        INTEGER, KIND :: k2
        INTEGER, LEN  :: l2

        TYPE(Base(k2,l2+2)) :: b_cmp
      END TYPE

      CONTAINS

      FUNCTION foo(Arg1, Arg2)
        CLASS(Base(4,*)) :: Arg1 , Arg2
        CLASS(Base(4,:)), POINTER :: foo

        Select_type_arg1: SELECT TYPE ( Arg1 )
              TYPEIS (Child(4,*,4,*))
                 Select_type_arg2: SELECT TYPE ( Arg2 )
                    TYPEIS (Base(4,*))
                       Allocate( foo, source = Arg2 )

                    CLASS DEFAULT
                       STOP 20
                 END SELECT Select_type_arg2

              TYPEIS (Base(4,*))
                 Allocate( foo, source = Arg1 )

              CLASS DEFAULT
                 STOP 21
        END SELECT Select_type_arg1

      END FUNCTION
END MODULE
PROGRAM FunctionResult08
      USE Mod
      IMPLICIT NONE

      CLASS(base(4,:)), POINTER :: b1, b2
      TYPE(child(4,2,4,6)), TARGET :: c1

      c1 = Child(4,2,4,6) ( 1, 'IBM', 2, ( Base(4,8) ( 3, 'XLF', 4) ) )

      b1 => c1
      Allocate( b2, source = ( Base(4,1) ( 8, 'A', 9) ))

      SELECT TYPE ( s => foo(b1,b2) )
        TYPE IS (Base(4,*))
          IF ( s%name   .NE. 'A' ) ERROR STOP 10
          IF ( ANY(s%A0 .NE.    8) ) ERROR STOP 11
          IF ( ANY(s%A1 .NE.    9) ) ERROR STOP 12
          IF ( .NOT. ASSOCIATED(foo(b1,b2)) ) ERROR STOP 13

        CLASS DEFAULT
           STOP 14
      END SELECT

      SELECT TYPE ( s => foo(b2,b1) )
        TYPE IS (Base(4,*))
          IF ( s%name   .NE. 'A' ) ERROR STOP 15
          IF ( ANY(s%A0 .NE.    8) ) ERROR STOP 16
          IF ( ANY(s%A1 .NE.    9) ) ERROR STOP 17
          IF ( .NOT. ASSOCIATED(foo(b2,b1)) ) ERROR STOP 18

        CLASS DEFAULT
           STOP 19
      END SELECT
END PROGRAM FunctionResult08
