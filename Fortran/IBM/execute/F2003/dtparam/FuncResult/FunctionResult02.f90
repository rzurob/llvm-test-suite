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

        INTEGER(k2) :: A0(l2), A1(l1), A2(l1+l2)
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

      SUBROUTINE Select_type(Arg)
        CLASS(*) :: Arg

        SELECT TYPE ( s => Arg )
          CLASS IS (Base(4,*))
            IF (s%k1 .NE.   4) STOP 10
            IF (s%l1 .NE.  10) STOP 11
            IF (LEN(s%C0)  .NE. 10) STOP 12
            IF (SIZE(s%F0) .NE. 10) STOP 13

          CLASS IS (Child(4,*,4,*))
            IF (s%k1 .NE.  4) STOP 20
            IF (s%l1 .NE.  8) STOP 21
            IF (LEN(s%C0)  .NE. 8) STOP 22
            IF (SIZE(s%F0) .NE. 8) STOP 23
            IF (s%k2 .NE.   4) STOP 24
            IF (s%l2 .NE.  16) STOP 25
            IF (SIZE(s%A0) .NE. 16) STOP 26
            IF (SIZE(s%A1) .NE.  8) STOP 27
            IF (SIZE(s%A2) .NE. 24) STOP 28

          CLASS IS (NextGen(4,*,4,*,*,*))
            IF (s%k1 .NE.   4) STOP 30
            IF (s%l1 .NE.  1) STOP 31
            IF (LEN(s%C0)  .NE. 1) STOP 32
            IF (SIZE(s%F0) .NE. 1) STOP 33
            IF (s%k2 .NE.  4) STOP 34
            IF (s%l2 .NE.  2) STOP 35
            IF (SIZE(s%A0) .NE. 2) STOP 36
            IF (SIZE(s%A1) .NE. 1) STOP 37
            IF (SIZE(s%A2) .NE. 3) STOP 38
            IF (s%l3 .NE.  3) STOP 39
            IF (s%l4 .NE.  6) STOP 40

          CLASS IS (Child(8,*,8,*))
            IF (s%k1 .NE.  8) STOP 50
            IF (s%l1 .NE.  2) STOP 51
            IF (LEN(s%C0)  .NE. 2) STOP 52
            IF (SIZE(s%F0) .NE. 2) STOP 53
            IF (s%k2 .NE.  8) STOP 54
            IF (s%l2 .NE.  4) STOP 55
            IF (SIZE(s%A0) .NE. 4) STOP 56
            IF (SIZE(s%A1) .NE. 2) STOP 57
            IF (SIZE(s%A2) .NE. 6) STOP 58

            IF ( s%C0 .NE. 'AB' ) STOP 59
            IF ( ANY(s%F0 .NEQV. .True.) ) STOP 60
            IF ( ANY(s%A0 .NE. -1) ) STOP 61
            IF ( ANY(s%A1 .NE. -2) ) STOP 62
            IF ( ANY(s%A2 .NE. -3) ) STOP 63

          CLASS DEFAULT
            STOP 100
        END SELECT
      END SUBROUTINE

END MODULE

PROGRAM FunctionResult02
      USE Mod
      IMPLICIT NONE

      CLASS(*), ALLOCATABLE :: u_poly

      ALLOCATE( Base(4,10) :: u_poly )

      ASSOCIATE ( a => func(u_poly) )
            call Select_type(a)
      END ASSOCIATE

      DEALLOCATE(u_poly)
      ALLOCATE( Child(4,8,4,16) :: u_poly )

      ASSOCIATE ( a => func(u_poly) )
            call Select_type(a)
      END ASSOCIATE

      DEALLOCATE(u_poly)
      ALLOCATE( NextGen(4,1,4,2,3,6) :: u_poly )

      ASSOCIATE ( a => func(u_poly) )
            call Select_type(a)
      END ASSOCIATE

      DEALLOCATE(u_poly)
      ALLOCATE( u_poly, SOURCE = Child(8,2,8,4) (C0 = 'AB', F0 = .True., A0 = -1, A1 = -2, A2 = -3) )

      ASSOCIATE ( a => func(u_poly) )
            call Select_type(a)
      END ASSOCIATE

END PROGRAM FunctionResult02
