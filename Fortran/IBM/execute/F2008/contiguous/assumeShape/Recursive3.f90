! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-10-18
!*
!*  PRIMARY FUNCTIONS TESTED   : CONTIGUOUS attribute
!*  SECONDARY FUNCTIONS TESTED :   DTP
!*
!*  DESCRIPTION                : - Recursive function call with assumed
!*                                 shape array that has the TARGET and
!*                                 CONTIGUOUS attribute
!*                               - Function result is a pointer with CONTIGUOUS
!*                                 attribute
!*                               - Actual argument is simply contiguous
!*                               - Associate construct
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
MODULE Mod
      IMPLICIT NONE

      TYPE :: Base(K1,N1)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1

        INTEGER(K1)   :: BaseId = 1
        CHARACTER(N1) :: C="ABC"
      END TYPE

      TYPE, EXTENDS(Base) :: Child
        INTEGER(K1)  :: ChildId = 2
      END TYPE

      CONTAINS

      RECURSIVE FUNCTION ReturnArr(Arg) RESULT(Res)
          TYPE(Child(4,*)), TARGET, CONTIGUOUS  :: Arg(:, :)
          TYPE(Child(4,:)), POINTER, CONTIGUOUS :: Res(:, :)
          LOGICAL, SAVE  :: Exit=.TRUE.

          Exit = .NOT. Exit

          IF (.NOT. Exit) THEN
            Res => ReturnArr(Arg)
          ELSE
            Res => Arg
          END IF
      END FUNCTION
END MODULE
PROGRAM Recursive3
      USE Mod
      IMPLICIT NONE

      TYPE(Child(4,10)) :: Arr(5, 5)

      Arr(1::1, 1::1) = Child(4,10)(ChildID=-2, BaseID=-1)
      IF ( .NOT. IS_CONTIGUOUS(Arr) ) STOP 10

      ASSOCIATE ( As => ReturnArr(Arr(1::1, 1::1)), As1 => ReturnArr(Arr(:, :3)) )
        IF ( .NOT. IS_CONTIGUOUS(As) )  STOP 20
        IF ( .NOT. IS_CONTIGUOUS(As1) ) STOP 21

        IF ( ANY (LBOUND(As)   .NE. [1,1]) )    STOP 22
        IF ( ANY (UBOUND(As)   .NE. [5,5]) )    STOP 23
        IF ( ANY (SHAPE(As)    .NE. [5,5]) )    STOP 24
        IF ( ANY (As%ChildID   .NE.    -2) )    STOP 25
        IF ( ANY (As%BaseID    .NE.    -1) )    STOP 26
        IF ( LEN(As%C)         .NE.     10 )    STOP 27
        IF ( TRIM(As(1,1)%C)   .NE.  "ABC" )    STOP 28

        ASSOCIATE( As => ReturnArr(As1(:,:)) )
          IF ( .NOT. IS_CONTIGUOUS(As) )  STOP 30

          IF ( ANY (LBOUND(As)  .NE. [1,1]) )    STOP 31
          IF ( ANY (UBOUND(As)  .NE. [5,3]) )    STOP 32
          IF ( ANY (SHAPE(As)   .NE. [5,3]) )    STOP 33
          IF ( ANY (As%ChildID  .NE.    -2) )    STOP 34
          IF ( ANY (As%BaseID   .NE.    -1) )    STOP 35
          IF ( LEN(As%C)        .NE.     10 )    STOP 36
          IF ( TRIM(As(1,1)%C)  .NE.  "ABC" )    STOP 37
        END ASSOCIATE
      END ASSOCIATE
END PROGRAM Recursive3
