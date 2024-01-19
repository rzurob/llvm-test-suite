!*  ===================================================================
!*
!*                               DTP-SELECT TYPE Construct
!*
!*  DATE                       : September 08, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : SELECT TYPE Construct - Derived-type parameters
!*  SECONDARY FUNCTIONS TESTED : Use association - Select case within select type
!*                               Selector being a function call
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
!* See defect 355886
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      MODULE Mod1
      IMPLICIT NONE
!*
      TYPE Node  (k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN :: l1

        CHARACTER(l1) :: tag="Empty" ! tags can be "Empty" or "Full"
        CLASS(Node(k1,l1)), POINTER :: Next => NULL()
      END TYPE Node

      TYPE, EXTENDS(Node) :: ExtNode
         INTEGER(k1), ALLOCATABLE :: my_arr(:)
      END TYPE ExtNode

      INTEGER, PARAMETER :: knd1 = 2 , len1 =10 , ZERO = 0
!*
      END MODULE Mod1
!*
      MODULE Mod2
      USE Mod1
      IMPLICIT NONE
!*
      CONTAINS
!*
      FUNCTION foo(Obj)
      CLASS(*), POINTER  :: foo
      CLASS(Node(k1=knd1,l1=len1)), POINTER :: Obj

      foo => Obj
      IF ( .NOT. ASSOCIATED(foo)) ERROR STOP 4

      END FUNCTION foo

      SUBROUTINE Sub1(T)
      CLASS(*) ::  T
      integer i

      Outer_SelType: SELECT TYPE (T)
        CLASS IS (ExtNode(knd1,*))
          IF ( .NOT. ASSOCIATED(T%Next)) ERROR STOP 5
          IF ( .NOT. ASSOCIATED(foo(T%Next))) ERROR STOP 6

          IF ( .NOT. ALLOCATED(T%my_arr)) ALLOCATE (T%my_arr(len1))
          IF ( .NOT. ALLOCATED(T%my_arr)) ERROR STOP 7

          Inner_SelType: SELECT TYPE ( A => foo(T%Next)) ! call to foo possible only within the select type
             TYPE IS (Node(knd1,*))
                IF (A%k1 .NE. knd1) ERROR STOP 112
                IF (A%l1 .NE. len1) ERROR STOP 113
                   ! select case
                      SELECT CASE (A%tag)
                        CASE ('Empty')
                          T%my_arr = (/ (0, I = 1, len1)/)
                        CASE ('Full')
                          T%my_arr = (/ (I, I = 1, len1)/)
                       CASE DEFAULT
                          STOP 114
                      END SELECT

             CLASS DEFAULT
                STOP 20

          END SELECT Inner_SelType

        CLASS IS (Node(knd1,*))
           STOP 31

        CLASS DEFAULT
           STOP 32

      END SELECT Outer_SelType

      END SUBROUTINE
!*
      END MODULE Mod2
!*
      PROGRAM Select_Type04c
      USE Mod1
      USE Mod2, ONLY: Sub1
      IMPLICIT NONE

      TYPE(ExtNode(knd1,len1))  :: ActiveNode
      TYPE(Node(knd1,len1)), TARGET :: FirstNode = (Node(knd1,len1) ())

      ActiveNode%Next  => FirstNode
      IF ( .NOT. ASSOCIATED(ActiveNode%Next)) ERROR STOP 12

      CALL Sub1(ActiveNode)
      IF (SIZE(ActiveNode%my_arr) .NE. len1) ERROR STOP 13
      IF (SUM(ActiveNode%my_arr) .NE. ZERO) ERROR STOP 14

      ActiveNode%Next%tag = 'Full'

      CALL Sub1(ActiveNode)
      IF (SIZE(ActiveNode%my_arr) .NE. len1) ERROR STOP 15
      IF (SUM(ActiveNode%my_arr) .NE. 55) ERROR STOP 16

      END PROGRAM Select_Type04c
