!* ===================================================================
!*
!* DATE                       : May 15, 2015
!* ORIGIN                     : AIX Compiler Development,
!*
!* PRIMARY FUNCTIONS TESTED   : ALLOCATE Statement with Source Expression
!* SECONDARY FUNCTIONS TESTED :
!*
!* REQUIRED COMPILER OPTIONS  :
!*
!* KEYWORD(S)                 :
!* TARGET(S)                  :
!* NUMBER OF TESTS CONDITIONS :
!*
!* DESCRIPTION                :
!*
!* TEST CASE ADAPTED FROM     : $(tsrcdir)/F2003/dtparam/allocate/SourceExp/AllocateWithSourceExp02.f
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

      TYPE(Child) :: c1, c2
      IF ( SIZE(c1%A1)     .NE.  10 ) ERROR STOP 10
      IF ( LBOUND(c1%A1,1) .NE.   1 ) ERROR STOP 11
      IF ( UBOUND(c1%A1,1) .NE.  10 ) ERROR STOP 12
      IF ( ANY(c1%A1       .NE. 16) ) ERROR STOP 13
      IF ( SIZE(c1%A2)     .NE.   5 ) ERROR STOP 14
      IF ( LBOUND(c1%A2,1) .NE.   2 ) ERROR STOP 15
      IF ( UBOUND(c1%A2,1) .NE.   6 ) ERROR STOP 16
      IF ( ANY(c1%A2       .NE.  8) ) ERROR STOP 17
      IF ( ASSOCIATED(c1%Cmp)       ) ERROR STOP 18

      ! TYPE(Child) :: c2
      IF ( SIZE(c2%A1)     .NE.  10 ) ERROR STOP 30
      IF ( LBOUND(c2%A1,1) .NE.   1 ) ERROR STOP 31
      IF ( UBOUND(c2%A1,1) .NE.  10 ) ERROR STOP 32
      IF ( ANY(c2%A1       .NE. 16) ) ERROR STOP 33
      IF ( SIZE(c2%A2)     .NE.   5 ) ERROR STOP 34
      IF ( LBOUND(c2%A2,1) .NE.   2 ) ERROR STOP 35
      IF ( UBOUND(c2%A2,1) .NE.   6 ) ERROR STOP 36
      IF ( ANY(c2%A2       .NE.  8) ) ERROR STOP 37
      IF ( ASSOCIATED(c2%Cmp)       ) ERROR STOP 38

      CALL sub2(c1, c2)

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
          IF ( ASSOCIATED(s%Cmp)       ) ERROR STOP 28

        CLASS DEFAULT
           ERROR STOP 29
      END SELECT

      SELECT TYPE ( s => c2%Cmp )
        CLASS IS (Child(4,*,4,*))
          IF ( SIZE(s%A1)     .NE.  10 ) ERROR STOP 40
          IF ( LBOUND(s%A1,1) .NE.   1 ) ERROR STOP 41
          IF ( UBOUND(s%A1,1) .NE.  10 ) ERROR STOP 42
          IF ( ANY(s%A1       .NE. 16) ) ERROR STOP 43
          IF ( SIZE(s%A2)     .NE.   5 ) ERROR STOP 44
          IF ( LBOUND(s%A2,1) .NE.   2 ) ERROR STOP 45
          IF ( UBOUND(s%A2,1) .NE.   6 ) ERROR STOP 46
          IF ( ANY(s%A2       .NE.  8) ) ERROR STOP 47
          IF ( ASSOCIATED(s%Cmp) )       ERROR STOP 48

        CLASS DEFAULT
           ERROR STOP 49
      END SELECT

      CONTAINS

      SUBROUTINE sub2(arg1, arg2)
        CLASS(base(4,*)) :: arg1, arg2
        CLASS(base(4,:)), POINTER :: tmp1
        CLASS(base(4,:)), POINTER :: tmp2

        SELECT TYPE ( arg1 )
          CLASS IS (Child(4,*,4,*))
            IF ( ASSOCIATED(arg1%Cmp) ) ERROR STOP 50
            SELECT TYPE ( arg2 )
              CLASS IS (Child(4,*,4,*))
                IF ( ASSOCIATED(arg1%Cmp) ) ERROR STOP 51
                ALLOCATE( tmp1, tmp2, SOURCE = arg1 )
                arg1%cmp => tmp1
                arg2%cmp => tmp2
              CLASS DEFAULT
                ERROR STOP 54
            END SELECT
          CLASS DEFAULT
             ERROR STOP 55
        END SELECT
      END SUBROUTINE sub2

END PROGRAM AllocateWithSourceExp02
