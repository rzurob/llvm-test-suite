!*  ===================================================================
!*
!*  DATE                       : January 20, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : ALLOCATE Statement with Source Expression
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
!*   ALLOCATE ( [ TYPE-spec :: ] allocation-list [, alloc-opt-list ] )
!*
!234567890123456789012345678901234567890123456789012345678901234567890
PROGRAM AllocateWithSourceExp02
      IMPLICIT NONE

      TYPE Base  (k1,l1)
        INTEGER, KIND :: k1 = 4
        INTEGER, LEN  :: l1 = 10

        INTEGER(k1) :: A1(l1) = k1**2
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (k2,l2)
        INTEGER, KIND :: k2 = 4
        INTEGER, LEN  :: l2 = 5

        CLASS(Base(k2,:)), POINTER :: Cmp => null()
        INTEGER(k2) :: A2(2:l1-l2+1) = k1+k2
      END TYPE Child

      TYPE(Child) :: c1

      IF ( SIZE(c1%A1)     .NE.  10 ) ERROR STOP 10
      IF ( LBOUND(c1%A1,1) .NE.   1 ) ERROR STOP 11
      IF ( UBOUND(c1%A1,1) .NE.  10 ) ERROR STOP 12
      IF ( ANY(c1%A1       .NE. 16) ) ERROR STOP 13
      IF ( SIZE(c1%A2)     .NE.   5 ) ERROR STOP 14
      IF ( LBOUND(c1%A2,1) .NE.   2 ) ERROR STOP 15
      IF ( UBOUND(c1%A2,1) .NE.   6 ) ERROR STOP 16
      IF ( ANY(c1%A2       .NE.  8) ) ERROR STOP 17
      IF ( ASSOCIATED(c1%Cmp) )       ERROR STOP 18

      CALL sub2(c1)

      SELECT TYPE ( s => c1%Cmp )
        CLASS IS (Child(4,*,4,*))
          IF ( SIZE(s%A1)     .NE.  10 ) ERROR STOP 20
          IF ( LBOUND(s%A1,1) .NE.   1 ) ERROR STOP 21
          IF ( UBOUND(s%A1,1) .NE.  10 ) ERROR STOP 22
          IF ( ANY(s%A1       .NE. 16) ) ERROR STOP 23
          IF ( SIZE(s%A2)     .NE.   5 ) ERROR STOP 24
          IF ( LBOUND(s%A2,1) .NE.   2 ) ERROR STOP 25
          IF ( UBOUND(s%A2,1) .NE.   6 ) ERROR STOP 26
          IF ( ANY(s%A2       .NE.  8) ) ERROR STOP 27
          IF ( ASSOCIATED(s%Cmp) ) ERROR STOP 28


        CLASS DEFAULT
           STOP 29
      END SELECT

      CONTAINS

      SUBROUTINE sub2(Arg)
        CLASS(Base(4,*)) :: Arg
        class(base(4,:)), pointer :: tmp

        SELECT TYPE ( Arg )
          CLASS IS (Child(4,*,4,*))
              IF ( ASSOCIATED(Arg%Cmp) ) ERROR STOP 40
!              ALLOCATE( Arg%Cmp, SOURCE = Arg )
              ALLOCATE( tmp, SOURCE = Arg )
              arg%cmp => tmp

          CLASS DEFAULT
             STOP 43
        END SELECT
      END SUBROUTINE sub2

END PROGRAM AllocateWithSourceExp02
