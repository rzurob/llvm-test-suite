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
!* Defect: 364759 and 364814
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
        PROCEDURE, PASS :: sub1 => CheckBase
      END TYPE

      TYPE, ABSTRACT, EXTENDS(Base)  :: Child(k2, l2)
        INTEGER(k1), KIND    :: k2 = k1
        INTEGER(k1), LEN     :: l2 = k1

        CHARACTER(l2+3) :: C1 = 'Child'
      END TYPE

      TYPE, EXTENDS(Child) :: NextGen(l3)
        INTEGER(k2), LEN     :: l3 = k2

        CHARACTER(l3)      :: C2 = CHAR(k2+64)
        INTEGER(k2)        :: A2 = k2
        TYPE(Base(k2, l3)) :: cmp
        CLASS(Base(k2, l3)), POINTER  :: ptr

        CONTAINS
        PROCEDURE, PASS :: sub2 => CheckNextGen
      END TYPE

      CONTAINS

      SUBROUTINE CheckBase(Arg)
        CLASS(Base(4,*)) :: Arg

        IF ( Arg%k1 .NE. 4 ) ERROR STOP 11
        IF ( Arg%l1 .NE. 3 ) ERROR STOP 12
        IF ( SIZE(Arg%A0) .NE. 3 ) ERROR STOP 13
        IF ( LEN(Arg%C0)  .NE. 3 ) ERROR STOP 14
        IF ( ANY(Arg%A0   .NE.   -1) ) ERROR STOP 15
        IF ( TRIM(Arg%C0) .NE. 'Bas' ) ERROR STOP 16

      END SUBROUTINE

      SUBROUTINE CheckNextGen(Arg)
        CLASS(NextGen(4,*,4,*,*)) :: Arg

        IF ( Arg%k1 .NE. 4 ) ERROR STOP 20
        IF ( Arg%l1 .NE. 3 ) ERROR STOP 21
        IF ( Arg%k2 .NE. 4 ) ERROR STOP 22
        IF ( Arg%l2 .NE. 3 ) ERROR STOP 23
        IF ( Arg%l3 .NE. 3 ) ERROR STOP 24

        IF ( SIZE(Arg%A0) .NE. 3 ) ERROR STOP 25
        IF ( LEN(Arg%C0)  .NE. 3 ) ERROR STOP 26
        IF ( LEN(Arg%C1)  .NE. 6 ) ERROR STOP 27
        IF ( LEN(Arg%C2)  .NE. 3 ) ERROR STOP 28

        IF ( ANY(Arg%A0   .NE.   -1) ) ERROR STOP 29
        IF ( TRIM(Arg%C0) .NE. 'Bas' ) ERROR STOP 30
        IF ( TRIM(Arg%C1) .NE. 'XYZ' ) ERROR STOP 31
        IF ( TRIM(Arg%C2) .NE. CHAR(Arg%k2+64) ) ERROR STOP 32
        IF ( Arg%A2       .NE. 1234 ) ERROR STOP 33

        IF ( Arg%cmp%k1 .NE. Arg%k2 ) ERROR STOP 34
        IF ( Arg%cmp%l1 .NE. Arg%l3 ) ERROR STOP 35
        IF ( SIZE(Arg%cmp%A0) .NE. Arg%l3 ) ERROR STOP 36
        IF ( LEN(Arg%cmp%C0)  .NE. Arg%l3 ) ERROR STOP 37
        IF ( ANY(Arg%cmp%A0   .NE.   99) ) ERROR STOP 38
        IF ( TRIM(Arg%cmp%C0) .NE. 'IBM' ) ERROR STOP 39

        IF ( ASSOCIATED(Arg%ptr) ) THEN
             CALL Arg%ptr%sub1()
        ENDIF

      END SUBROUTINE

END MODULE
PROGRAM DTP_PARAMETER_06
      USE Mod

      TYPE(Base(4,3)), PARAMETER :: b1 = Base(4,3)()

      TYPE(NextGen(4,3,4,3,3)), PARAMETER :: n1 = NextGen(4,3,4,3,3)       &
          ( C1 = 'XYZ', A2 = 1234, ptr = null(), cmp = Base(4,3)(99, 'IBM') )

      TYPE(Base(4,3)), TARGET :: btgt = b1
      TYPE(NextGen(4,3,4,3,3)), TARGET :: ntgt = n1
      CLASS(Base(4,:)), POINTER :: poly

      CALL b1%sub1()
      CALL btgt%sub1()

      poly => btgt
      CALL poly%sub1()

      CALL n1%sub2()

      ntgt%ptr => btgt
      CALL ntgt%sub2()

      poly => ntgt
      SELECT TYPE ( poly )
          CLASS IS (NextGen(4,*,4,*,*))
             IF ( ASSOCIATED(poly%ptr) .nEQV. ASSOCIATED(ntgt%ptr) ) ERROR STOP 60
            CALL poly%sub2()

          CLASS DEFAULT
             STOP 100
      END SELECT

END PROGRAM DTP_PARAMETER_06
