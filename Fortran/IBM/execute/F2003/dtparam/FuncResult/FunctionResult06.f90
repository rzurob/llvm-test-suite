!*  ===================================================================
!*
!*  DATE                       : March 25, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Function result - unlimited poly
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
!* Defect: 355394.3
!*
!234567890123456789012345678901234567890123456789012345678901234567890
MODULE Mod
      IMPLICIT NONE

      TYPE Base (k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN  :: l1

        INTEGER(k1) :: A0(l1), A1(2*l1), A2(l1,l1)
      END TYPE

      TYPE,  EXTENDS(Base) :: Child (k2,l2)
        INTEGER, KIND :: k2
        INTEGER, LEN  :: l2

        TYPE(Base(k2,2*l2+1)) :: b_cmp
      END TYPE

      TYPE,  EXTENDS(Child) :: NextGen (k3,l3)
        INTEGER, KIND :: k3
        INTEGER, LEN  :: l3

        TYPE(Child(k3,l3+2,k3,l3+3)) :: c_cmp
      END TYPE

      CONTAINS

      CLASS(*) FUNCTION BuildUpoly(Arg)
        CLASS(*) :: Arg
        POINTER :: BuildUpoly

        ALLOCATE( BuildUpoly, SOURCE = Arg )

      END FUNCTION
END MODULE

PROGRAM FunctionResult06
      USE Mod
      IMPLICIT NONE

      CLASS(*), POINTER :: upoly

      upoly => BuildUpoly( Base(4,10) ( 1, 2, 3 ) )
      SELECT TYPE ( upoly )
        CLASS IS (Base(4,*))
            IF (upoly%l1 .NE. 10) STOP 10
            IF (SIZE(upoly%A0) .NE.   10) STOP 11
            IF (ANY(upoly%A0   .NE.   1)) STOP 12
            IF (SIZE(upoly%A1) .NE.   20) STOP 13
            IF (ANY(upoly%A1   .NE.   2)) STOP 14
            IF (SIZE(upoly%A2) .NE.  100) STOP 15
            IF (ANY(upoly%A2   .NE.   3)) STOP 16

        CLASS DEFAULT
           STOP 17
      END SELECT

      upoly => BuildUpoly( Child(4,3,4,6) ( 10, 20, 30, Base(4,13) ( 100, 200, 300 ) ) )
      SELECT TYPE ( upoly )
        CLASS IS (Child(4,*,4,*))
            IF (upoly%l1 .NE. 3) STOP 18
            IF (upoly%l2 .NE. 6) STOP 19
            IF (SIZE(upoly%A0) .NE.   3) STOP 20
            IF (ANY(upoly%A0   .NE. 10)) STOP 21
            IF (SIZE(upoly%A1) .NE.   6) STOP 22
            IF (ANY(upoly%A1   .NE. 20)) STOP 23
            IF (SIZE(upoly%A2) .NE.   9) STOP 24
            IF (ANY(upoly%A2   .NE. 30)) STOP 25

            IF (upoly%b_cmp%l1 .NE. 13) STOP 26
            IF (SIZE(upoly%b_cmp%A0) .NE.   13) STOP 27
            IF (ANY(upoly%b_cmp%A0   .NE. 100)) STOP 28
            IF (SIZE(upoly%b_cmp%A1) .NE.   26) STOP 29
            IF (ANY(upoly%b_cmp%A1   .NE. 200)) STOP 30
            IF (SIZE(upoly%b_cmp%A2) .NE.  169) STOP 31
            IF (ANY(upoly%b_cmp%A2   .NE. 300)) STOP 32

        CLASS DEFAULT
           STOP 33
      END SELECT

      upoly => BuildUpoly( NextGen(4,8,4,4,4,2)( 1, 2, 3, Base(4,9) (10, 20, 30), &
         & Child(4,4,4,5) ( 100, 200, 300, Base(4,11) (11, 22, 33) ) ) )
      SELECT TYPE ( upoly )
        CLASS IS (NextGen(4,*,4,*,4,*))
            IF (upoly%l1 .NE. 8) STOP 40
            IF (upoly%l2 .NE. 4) STOP 41
            IF (SIZE(upoly%A0) .NE.   8) STOP 42
            IF (ANY(upoly%A0   .NE.  1)) STOP 43
            IF (SIZE(upoly%A1) .NE.  16) STOP 44
            IF (ANY(upoly%A1   .NE.  2)) STOP 45
            IF (SIZE(upoly%A2) .NE.  64) STOP 46
            IF (ANY(upoly%A2   .NE.  3)) STOP 47

            IF (upoly%b_cmp%l1 .NE. 9) STOP 48
            IF (SIZE(upoly%b_cmp%A0) .NE.   9) STOP 49
            IF (ANY(upoly%b_cmp%A0   .NE. 10)) STOP 50
            IF (SIZE(upoly%b_cmp%A1) .NE.  18) STOP 51
            IF (ANY(upoly%b_cmp%A1   .NE. 20)) STOP 52
            IF (SIZE(upoly%b_cmp%A2) .NE.  81) STOP 53
            IF (ANY(upoly%b_cmp%A2   .NE. 30)) STOP 54

            IF (upoly%c_cmp%l1 .NE. 4) STOP 55
            IF (upoly%c_cmp%l2 .NE. 5) STOP 56
            IF (SIZE(upoly%c_cmp%A0) .NE.    4) STOP 57
            IF (ANY(upoly%c_cmp%A0   .NE. 100)) STOP 58
            IF (SIZE(upoly%c_cmp%A1) .NE.   8) STOP 59
            IF (ANY(upoly%c_cmp%A1   .NE. 200)) STOP 60
            IF (SIZE(upoly%c_cmp%A2) .NE.   16) STOP 61
            IF (ANY(upoly%c_cmp%A2   .NE. 300)) STOP 62

            IF (upoly%c_cmp%b_cmp%l1 .NE. 11) STOP 63
            IF (SIZE(upoly%c_cmp%b_cmp%A0) .NE.  11) STOP 64
            IF (ANY(upoly%c_cmp%b_cmp%A0   .NE. 11)) STOP 65
            IF (SIZE(upoly%c_cmp%b_cmp%A1) .NE.  22) STOP 66
            IF (ANY(upoly%c_cmp%b_cmp%A1   .NE. 22)) STOP 67
            IF (SIZE(upoly%c_cmp%b_cmp%A2) .NE. 121) STOP 68
            IF (ANY(upoly%c_cmp%b_cmp%A2   .NE. 33)) STOP 69

        CLASS DEFAULT
           STOP 70
      END SELECT

END PROGRAM FunctionResult06
