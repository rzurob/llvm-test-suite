!*  ===================================================================
!*
!*  DATE                       : June 14, 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : Specification expression - Host Association
!*  SECONDARY FUNCTIONS TESTED : Named Constant
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*  An object designator with a base object that is made accessible by host association
!*
!234567890123456789012345678901234567890123456789012345678901234567890
MODULE Mod
      IMPLICIT NONE

      TYPE Base (k1,l1)
         INTEGER, KIND :: k1 = 4
         INTEGER, LEN  :: l1 = 1

         INTEGER(k1) :: I1, A1(l1)
         CHARACTER(l1) :: C1
      END TYPE
END MODULE
PROGRAM SpecExpHostAssociation01b
      USE Mod
      IMPLICIT TYPE (Base) (b)

      PARAMETER (b1 = Base ( 10, 20 , 'B' ))
      TYPE(Base(8,3)), PARAMETER :: d1 = Base(8,3) (I1 = 11, A1 = 22, C1 = 'IBM')

      CALL Sub11(1)
      CALL Sub12(4)
      CALL Sub13(10)
      CALL Sub14(20)
      CALL Sub15(4)
      CALL Sub16(1)

      CALL Sub21(3)
      CALL Sub22(8)
      CALL Sub23(11)
      CALL Sub24(22)
      CALL Sub25(8)
      CALL Sub26(3)

      CONTAINS

      SUBROUTINE Sub11(N)
        INTEGER :: N
        TYPE(Base(l1=b1%l1)) :: Obj

         IF ( Obj%k1 .NE.  4 ) STOP 10
         IF ( Obj%l1 .NE.  N ) STOP 11
         IF ( SIZE(Obj%A1) .NE.   N ) STOP 12
         IF ( LEN(Obj%C1)  .NE.   N ) STOP 13
      END SUBROUTINE Sub11

      SUBROUTINE Sub12(N)
        INTEGER :: N
        TYPE(Base(l1=b1%k1)) :: Obj

         IF ( Obj%k1 .NE. 4 ) STOP 14
         IF ( Obj%l1 .NE. N ) STOP 15
         IF ( SIZE(Obj%A1) .NE.  N ) STOP 16
         IF ( LEN(Obj%C1)  .NE.  N ) STOP 17
      END SUBROUTINE Sub12

      SUBROUTINE Sub13(N)
        INTEGER :: N
        TYPE(Base(l1=b1%I1)) :: Obj

         IF ( Obj%k1 .NE. 4 ) STOP 18
         IF ( Obj%l1 .NE. N ) STOP 19
         IF ( SIZE(Obj%A1) .NE.  N ) STOP 20
         IF ( LEN(Obj%C1)  .NE.  N ) STOP 21
      END SUBROUTINE Sub13

      SUBROUTINE Sub14(N)
        INTEGER :: N
        TYPE(Base(l1=b1%A1(1))) :: Obj

         IF ( Obj%k1 .NE. 4 ) STOP 22
         IF ( Obj%l1 .NE. N ) STOP 23
         IF ( SIZE(Obj%A1) .NE.  N ) STOP 24
         IF ( LEN(Obj%C1)  .NE.  N ) STOP 25
      END SUBROUTINE Sub14

      SUBROUTINE Sub15(N)
        INTEGER :: N
        TYPE(Base(l1=KIND(b1%I1))) :: Obj

         IF ( Obj%k1 .NE. 4 ) STOP 26
         IF ( Obj%l1 .NE. N ) STOP 27
         IF ( SIZE(Obj%A1) .NE.  N ) STOP 28
         IF ( LEN(Obj%C1)  .NE.  N ) STOP 29
      END SUBROUTINE Sub15

      SUBROUTINE Sub16(N)
        INTEGER :: N
        TYPE(Base(l1=LEN(b1%C1))) :: Obj

         IF ( Obj%k1 .NE. 4 ) STOP 26
         IF ( Obj%l1 .NE. N ) STOP 27
         IF ( SIZE(Obj%A1) .NE.  N ) STOP 28
         IF ( LEN(Obj%C1)  .NE.  N ) STOP 29
      END SUBROUTINE Sub16

      SUBROUTINE Sub21(N)
        INTEGER :: N
        TYPE(Base(l1=d1%l1)) :: Obj

         IF ( Obj%k1 .NE. 4 ) STOP 30
         IF ( Obj%l1 .NE. N ) STOP 31
         IF ( SIZE(Obj%A1) .NE.  N ) STOP 32
         IF ( LEN(Obj%C1)  .NE.  N ) STOP 33
      END SUBROUTINE Sub21

      SUBROUTINE Sub22(N)
        INTEGER :: N
        TYPE(Base(l1=d1%k1)) :: Obj

         IF ( Obj%k1 .NE. 4 ) STOP 34
         IF ( Obj%l1 .NE. N ) STOP 35
         IF ( SIZE(Obj%A1) .NE.  N ) STOP 36
         IF ( LEN(Obj%C1)  .NE.  N ) STOP 37
      END SUBROUTINE Sub22

      SUBROUTINE Sub23(N)
        INTEGER :: N
        TYPE(Base(l1=d1%I1)) :: Obj

         IF ( Obj%k1 .NE. 4 ) STOP 38
         IF ( Obj%l1 .NE. N ) STOP 39
         IF ( SIZE(Obj%A1) .NE.  N ) STOP 40
         IF ( LEN(Obj%C1)  .NE.  N ) STOP 41
      END SUBROUTINE Sub23

      SUBROUTINE Sub24(N)
        INTEGER :: N
        TYPE(Base(l1=d1%A1(1))) :: Obj

         IF ( Obj%k1 .NE. 4 ) STOP 42
         IF ( Obj%l1 .NE. N ) STOP 43
         IF ( SIZE(Obj%A1) .NE.  N ) STOP 44
         IF ( LEN(Obj%C1)  .NE.  N ) STOP 45
      END SUBROUTINE Sub24

      SUBROUTINE Sub25(N)
        INTEGER :: N
        TYPE(Base(l1=KIND(d1%I1))) :: Obj

         IF ( Obj%k1 .NE. 4 ) STOP 26
         IF ( Obj%l1 .NE. N ) STOP 27
         IF ( SIZE(Obj%A1) .NE.  N ) STOP 28
         IF ( LEN(Obj%C1)  .NE.  N ) STOP 29
      END SUBROUTINE Sub25

      SUBROUTINE Sub26(N)
        INTEGER :: N
        TYPE(Base(l1=LEN(d1%C1))) :: Obj

         IF ( Obj%k1 .NE. 4 ) STOP 46
         IF ( Obj%l1 .NE. N ) STOP 47
         IF ( SIZE(Obj%A1) .NE.  N ) STOP 48
         IF ( LEN(Obj%C1)  .NE.  N ) STOP 49
      END SUBROUTINE Sub26

END PROGRAM SpecExpHostAssociation01b
