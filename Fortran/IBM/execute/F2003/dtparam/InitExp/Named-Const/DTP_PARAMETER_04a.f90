!*  ===================================================================
!*
!*  DATE                       : April 24, 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : PARAMETER
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

      TYPE :: Base(k1, l1)
        INTEGER, KIND :: k1 = 1
        INTEGER, LEN  :: l1 = 1

        INTEGER(k1)   :: A0(l1) = -1
        CHARACTER(l1) :: C0 = 'Base'

        CONTAINS
        PROCEDURE, PASS(Obj) :: sub1 => CheckBase
      END TYPE

      TYPE, ABSTRACT, EXTENDS(Base)  :: Child(k2, l2)
        INTEGER(k1), KIND    :: k2 = k1
        INTEGER(k1), LEN     :: l2 = k1

        CHARACTER(l2+3) :: C1 = 'Child'
      END TYPE

      TYPE, EXTENDS(Child) :: NextGen(l3)
        INTEGER(k2), LEN     :: l3 = k2

        CHARACTER(l3)      :: C2 = CHAR(k2)
        INTEGER(k2)        :: A2 = k2
        TYPE(Base(k2, l3)) :: cmp
        CLASS(Base(k2, l3)), POINTER  :: ptr

        CONTAINS
        PROCEDURE, PASS(Obj) :: sub2 => CheckNextGen
      END TYPE

      CONTAINS

      SUBROUTINE CheckBase(Obj,Arg)
        CLASS(Base(4,*)) :: Obj
        TYPE (Base(4,*)) :: Arg

        IF ( Arg%k1 .NE. Obj%k1 ) STOP 11
        IF ( Arg%l1 .NE. Obj%l1 ) STOP 12
        IF ( SIZE(Arg%A0) .NE. SIZE(Obj%A0) ) STOP 13
        IF ( LEN(Arg%C0)  .NE.  LEN(Obj%C0) ) STOP 14
        IF ( ANY(Arg%A0   .NE.      Obj%A0) ) STOP 15
        IF ( TRIM(Arg%C0) .NE. TRIM(Obj%C0) ) STOP 16
      END SUBROUTINE

      SUBROUTINE CheckNextGen(Obj,Arg)
      CLASS(NextGen(4,*,4,*,*)) :: Obj
      TYPE (NextGen(4,*,4,*,*)) :: Arg

        IF ( Arg%k1 .NE. Obj%k1 ) STOP 20
        IF ( Arg%l1 .NE. Obj%l1 ) STOP 21
        IF ( Arg%k2 .NE. Obj%k2 ) STOP 22
        IF ( Arg%l2 .NE. Obj%l2 ) STOP 23
        IF ( Arg%l3 .NE. Obj%l3 ) STOP 24

        IF ( SIZE(Arg%A0) .NE. SIZE(Obj%A0) ) STOP 25
        IF ( LEN(Arg%C0)  .NE.  LEN(Obj%C0) ) STOP 26
        IF ( LEN(Arg%C1)  .NE.  LEN(Obj%C1) ) STOP 27
        IF ( LEN(Arg%C2)  .NE.  LEN(Obj%C2) ) STOP 28

        IF ( ANY(Arg%A0   .NE.      Obj%A0) ) STOP 29
        IF ( TRIM(Arg%C0) .NE. TRIM(Obj%C0) ) STOP 30
        IF ( TRIM(Arg%C1) .NE. TRIM(Obj%C1) ) STOP 31
        IF ( TRIM(Arg%C2) .NE. TRIM(Obj%C2) ) STOP 32
        IF ( Arg%A2       .NE.       Obj%A2 ) STOP 33

        IF ( Arg%cmp%k1 .NE. Obj%cmp%k1 ) STOP 34
        IF ( Arg%cmp%l1 .NE. Obj%cmp%l1 ) STOP 35
        IF ( SIZE(Arg%cmp%A0) .NE. SIZE(Obj%cmp%A0) ) STOP 36
        IF ( LEN(Arg%cmp%C0)  .NE.  LEN(Obj%cmp%C0) ) STOP 37
        IF ( ANY(Arg%cmp%A0   .NE.      Obj%cmp%A0) ) STOP 38
        IF ( TRIM(Arg%cmp%C0) .NE. TRIM(Obj%cmp%C0) ) STOP 39

        IF ( ASSOCIATED(Arg%ptr) .nEQV. ASSOCIATED(Obj%ptr) ) STOP 40

      END SUBROUTINE

END MODULE
PROGRAM DTP_PARAMETER_04a
      USE Mod

      TYPE(Base(4,2)), PARAMETER :: b1 = Base(4,2)()
      TYPE(Base(4,2)), PARAMETER :: b2 = b1, b3 = b2

      TYPE(NextGen(4,3,4,3,3)), PARAMETER :: n1 = NextGen(4,3,4,3,3)       &
          ( C1 = "XYZ", C2 = "ZYX", A2 = 1234, ptr = null(), cmp = Base(4,3)() )
      TYPE(NextGen(4,3,4,3,3)), PARAMETER :: n2 = n1, n3 = n2

      TYPE(Base(4,2)), TARGET :: btgt = b3
      TYPE(NextGen(4,3,4,3,3)), TARGET :: ntgt = n3
      CLASS(Base(4,:)), POINTER :: poly

      CALL b1%sub1(b1)
      CALL b1%sub1(b2)
      CALL b1%sub1(b3)
      CALL b2%sub1(b1)
      CALL b2%sub1(b2)
      CALL b2%sub1(b3)
      CALL b3%sub1(b1)
      CALL b3%sub1(b2)
      CALL b3%sub1(b3)

      CALL n1%sub2(n1)
      CALL n1%sub2(n2)
      CALL n1%sub2(n3)
      CALL n2%sub2(n1)
      CALL n2%sub2(n2)
      CALL n2%sub2(n3)
      CALL n3%sub2(n1)
      CALL n3%sub2(n2)
      CALL n3%sub2(n3)

      poly => btgt
      CALL poly%sub1(btgt)
      CALL poly%sub1(b3)

      poly => ntgt
      SELECT TYPE ( poly )
          CLASS IS (NextGen(4,*,4,*,*))
            CALL poly%sub2(ntgt)
            CALL poly%sub2(n3)

          CLASS DEFAULT
             STOP 100
      END SELECT

END PROGRAM DTP_PARAMETER_04a
