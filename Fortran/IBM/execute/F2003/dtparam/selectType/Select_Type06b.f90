!*  ===================================================================
!*
!*                               DTP-SELECT TYPE Construct
!*
!*  DATE                       : August 12, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : SELECT TYPE Construct - Derived-type parameters
!*  SECONDARY FUNCTIONS TESTED : generic type-bound procedures
!*                               Use association
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

        REAL(k1), ALLOCATABLE ::  my_arr(:)
      END TYPE Base

      END MODULE Mod1
!*####################################################################
      MODULE Mod2
      USE Mod1
      IMPLICIT NONE

      INTEGER, PARAMETER :: single = KIND(0.0), double = KIND(0d0), len1 = 10

      TYPE, EXTENDS(Base) :: List
        TYPE(List(k1,l1)), POINTER :: Next => NULL()
        CONTAINS
        PROCEDURE, PRIVATE :: Dfoo
        PROCEDURE, PRIVATE :: Sfoo
        GENERIC :: p => Sfoo , Dfoo
      END TYPE List

      CONTAINS

      TYPE(List(single,len1)) FUNCTION Sfoo (Obj) result (answer)
        CLASS(List(single,*)), TARGET  :: OBJ
        POINTER :: answer

           answer => OBJ%NEXT

      END FUNCTION Sfoo

      TYPE(List(double,len1)) FUNCTION Dfoo (Obj) result (answer)
        CLASS(List(double,*)), TARGET  :: OBJ
        POINTER :: answer

           answer => OBJ%NEXT

      END FUNCTION Dfoo

      END MODULE Mod2
!*####################################################################
      MODULE Mod3
      USE Mod2
      IMPLICIT CLASS(List(single,len1))(S)
      IMPLICIT CLASS(List(double,len1))(D)

      INTERFACE BuildList
        MODULE PROCEDURE Build_simple_List , Build_double_List
      END INTERFACE

      CONTAINS

      SUBROUTINE Build_simple_List(S)
        POINTER :: SNode1, SNode2 , S

       SNode1 => S

       DO I = 1, 10
         ALLOCATE(SNode2)
         SNode2%my_arr = (/ (REAL(K) , K = 1, I)/)
         SNode1%Next => SNode2
         SNode1 => SNode2
       END DO

      END SUBROUTINE Build_simple_List

      SUBROUTINE Build_double_List(D)
        POINTER :: DNode1, DNode2 , D

       DNode1 => D

       DO I = 1, 10
         ALLOCATE(DNode2)
         DNode2%my_arr = (/ (REAL(K) , K = 1, I)/)
         DNode1%Next => DNode2
         DNode1 => DNode2
       END DO

      END SUBROUTINE Build_double_List

      END MODULE Mod3
!*####################################################################
      PROGRAM Select_Type06b
      USE Mod3, ONLY: List, BuildList
      INTEGER, PARAMETER :: knd1 = KIND(0.0), knd2 = KIND(0d0), len1 = 10
      IMPLICIT CLASS(List(knd1,len1))(S)
      IMPLICIT CLASS(List(knd2,len1))(D)

      CLASS(*), POINTER :: U
      POINTER :: SInit, DInit

      ALLOCATE(List(knd1,len1):: SInit)
      IF ( .NOT. ASSOCIATED(SInit)) STOP 10
      ALLOCATE(List(knd2,len1):: DInit)
      IF ( .NOT. ASSOCIATED(DInit)) STOP 11

      CALL BuildList(SInit)
      U => SInit

      CALL SELECT_TYPE(U)
      CALL BuildList(DInit)
      U => DInit

      CALL SELECT_TYPE(U)

      CONTAINS

      SUBROUTINE SELECT_TYPE(Obj)
      CLASS(*), POINTER :: Obj
      POINTER :: STEMP, DTEMP

      SELECT TYPE ( Obj )
        CLASSIS (List(knd1,*))

          STEMP => Obj

          DO I = 1, 10
           IF ( .NOT. ASSOCIATED(STEMP%NEXT) ) STOP 20
           STEMP => STEMP%p()
           IF ( SIZE(STEMP%my_arr) .NE. I ) STOP 30
          END DO

        CLASSIS (List(knd2,*))

          DTEMP => Obj

          DO I = 1, 10
           IF ( .NOT. ASSOCIATED(DTEMP%NEXT) ) STOP 22
           DTEMP => DTEMP%p()
           IF ( SIZE(DTEMP%my_arr) .NE. I ) STOP 31
          END DO

        CLASSDEFAULT
           STOP 24

      END SELECT

      END SUBROUTINE SELECT_TYPE

      END PROGRAM Select_Type06b
