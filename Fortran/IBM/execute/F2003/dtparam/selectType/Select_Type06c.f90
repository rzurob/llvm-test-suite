!*  ===================================================================
!*
!*                               DTP-SELECT TYPE Construct
!*
!*  DATE                       : August 12, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : SELECT TYPE Construct - Derived-type parameters
!*  SECONDARY FUNCTIONS TESTED : type-bound procedures - Use association
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : SELECT TYPE Construct
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*  8.1.5.1 Form of the SELECT TYPE construct
!*
!*  R821 select-type-construct  is  select-type-stmt
!*                                      [ type-guard-stmt
!*                                        block ] ...
!*                                      end-select-type-stmt
!*  R822 select-type-stmt       is  [ select-construct-name : ] SELECT TYPE&
!*                                      &( [ associate-name => ] selector )
!*
!*  R823 type-guard-stmt is TYPE IS ( type-spec ) [ select-construct-name ]
!*                       or CLASS IS ( type-spec ) [ select-construct-name ]
!*                       or CLASS DEFAULT [ select-construct-name ]
!*
!* See defect 335952
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      MODULE Mod1
      IMPLICIT NONE

      TYPE Base  (k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN :: l1

        CHARACTER(l1), ALLOCATABLE :: tag(:)
      END TYPE Base

      TYPE, EXTENDS(Base) :: List
        TYPE(List(k1,l1)), POINTER :: Next => NULL()
        CONTAINS
        PROCEDURE  :: p=> foo
      END TYPE List

      INTEGER, PARAMETER :: knd1 = KIND(0.0), len1 = 10

      CONTAINS

      TYPE(List(knd1,len1)) FUNCTION foo (Obj) result (answer)
        CLASS(List(knd1,*)), TARGET  :: OBJ
        POINTER :: answer

           answer => OBJ%NEXT

      END FUNCTION foo

      END MODULE Mod1
!*####################################################################
      MODULE Mod2
      USE Mod1
      IMPLICIT CLASS(List(knd1,len1))(S)

      CONTAINS

      SUBROUTINE BuildList(S)
        POINTER :: SNode1, SNode2 , S

       SNode1 => S

       DO I = 1, 10
         ALLOCATE(SNode2)
         SNode2%tag = (/ (CHAR(K) , K = 65, 74)/)
         SNode1%Next => SNode2
         SNode1 => SNode2
       END DO

      END SUBROUTINE BuildList

      END MODULE Mod2
!*####################################################################
      PROGRAM Select_Type06c
      USE Mod2, ONLY: List, BuildList
      INTEGER, PARAMETER :: knd1 = KIND(0.0), len1 = 10
      IMPLICIT CLASS(List(knd1,len1))(S)

      CLASS(*), POINTER :: U
      POINTER :: SInit

      ALLOCATE(List(knd1,len1):: SInit)
      IF ( .NOT. ASSOCIATED(SInit)) STOP 10

      CALL BuildList(SInit)
      U => SInit

      CALL SELECT_TYPE(U)

      CONTAINS

      SUBROUTINE SELECT_TYPE(Obj)
      CLASS(*), POINTER :: Obj
      POINTER :: STEMP

      SELECT TYPE ( Obj )
        CLASSIS (List(knd1,*))

          STEMP => Obj

          DO I = 1, 10
           IF ( .NOT. ASSOCIATED(STEMP%NEXT) ) STOP 20
           STEMP => STEMP%p()
           IF ( LEN(STEMP%tag) .NE. len1 ) STOP 30
           IF ( STEMP%tag(1) .NE. 'A' ) STOP 31
           IF ( STEMP%tag(10) .NE. 'J' ) STOP 32
          END DO

        CLASSDEFAULT
           STOP 40

      END SELECT

      END SUBROUTINE SELECT_TYPE

      END PROGRAM Select_Type06c
