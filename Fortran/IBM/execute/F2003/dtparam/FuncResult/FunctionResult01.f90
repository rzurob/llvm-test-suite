!*  ===================================================================
!*
!*  DATE                       : March 15, 2008
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
!*  Defect 356663
!234567890123456789012345678901234567890123456789012345678901234567890
MODULE Mod
      IMPLICIT NONE

      TYPE Base (k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN  :: l1

        CHARACTER(l1) :: C0 = "0"
        LOGICAL       :: F0(l1) = .True.
      END TYPE

      TYPE,  EXTENDS(Base) :: Child (k2,l2)
        INTEGER, KIND :: k2
        INTEGER, LEN  :: l2

        INTEGER(k2) :: A0(l2), A1(l1), A3(l1+l2)
      END TYPE

      TYPE, EXTENDS(Child) :: NextGen (l3,l4)
        INTEGER, LEN  :: l3
        INTEGER, LEN  :: l4
      END TYPE

      CONTAINS

      FUNCTION func(Arg)
        CLASS(*) :: Arg, func
        POINTER  :: func

          ALLOCATE(func, SOURCE = Arg)

      END FUNCTION
END MODULE

PROGRAM FunctionResult01
      USE Mod
      IMPLICIT NONE

      TYPE(Base(4,10)) :: b1
      TYPE(Base(4,:)), POINTER :: b_ptr
      CLASS(Base(4,:)), POINTER :: b_poly

      SELECT TYPE ( s => func(b1) )
        TYPE IS (Base(4,*))
           IF (s%k1 .NE.  4) STOP 10
           IF (s%l1 .NE. 10) STOP 11
           IF (LEN(s%C0) .NE. 10) STOP 12
           IF (SIZE(s%F0) .NE. 10) STOP 13

        CLASS DEFAULT
           STOP 14
      END SELECT

      ALLOCATE( Base(4,15) :: b_ptr )
      SELECT TYPE ( s => func(b_ptr) )
        TYPE IS (Base(4,*))
           IF (s%k1 .NE.  4) STOP 15
           IF (s%l1 .NE. 15) STOP 16
           IF (LEN(s%C0) .NE. 15) STOP 17
           IF (SIZE(s%F0) .NE. 15) STOP 18

        CLASS DEFAULT
           STOP 19
      END SELECT

      ALLOCATE( NextGen(4,1,4,2,5,10) :: b_poly )
      SELECT TYPE ( s => func(b_poly) )
        CLASS IS (NextGen(4,*,4,*,*,*))
           IF (s%k1 .NE.  4) STOP 20
           IF (s%l1 .NE.  1) STOP 21
           IF (LEN(s%C0) .NE. 1) STOP 22
           IF (SIZE(s%F0) .NE. 1) STOP 23
           IF (s%k2 .NE.  4) STOP 24
           IF (s%l2 .NE.  2) STOP 25
           IF (SIZE(s%A0) .NE. 2) STOP 26
           IF (SIZE(s%A1) .NE. 1) STOP 27
           IF (SIZE(s%A3) .NE. 3) STOP 28
           IF (s%l3 .NE.  5) STOP 29
           IF (s%l4 .NE. 10) STOP 30

        CLASS DEFAULT
           STOP 31
      END SELECT

      ALLOCATE( Child(4,10,4,20) :: b_poly )
      SELECT TYPE ( s => func(b_poly) )
        CLASS IS (Child(4,*,4,*))
           IF (s%k1 .NE.   4) STOP 32
           IF (s%l1 .NE.  10) STOP 33
           IF (LEN(s%C0)  .NE. 10) STOP 34
           IF (SIZE(s%F0) .NE. 10) STOP 35
           IF (s%k2 .NE.   4) STOP 36
           IF (s%l2 .NE.  20) STOP 37
           IF (SIZE(s%A0) .NE. 20) STOP 38
           IF (SIZE(s%A1) .NE. 10) STOP 39
           IF (SIZE(s%A3) .NE. 30) STOP 40

        CLASS DEFAULT
           STOP 41
      END SELECT

      DEALLOCATE(b_poly,b_ptr)
END PROGRAM FunctionResult01
