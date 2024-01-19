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

        CHARACTER(l1) :: C0 = '0'
        LOGICAL       :: F0(l1) = .True.
      END TYPE

      TYPE,  EXTENDS(Base) :: Child (k2,l2)
        INTEGER, KIND :: k2
        INTEGER, LEN  :: l2

        INTEGER(k2) :: A0(l2) = -1
      END TYPE

      TYPE, EXTENDS(Child) :: NextGen (l3,l4)
        INTEGER, LEN  :: l3
        INTEGER, LEN  :: l4

        CLASS(*), POINTER :: Ptr
      END TYPE

      CONTAINS

      FUNCTION func(Arg)
        CLASS(*) :: Arg, func
        POINTER  :: func

          ALLOCATE(func, SOURCE = Arg)

      END FUNCTION
END MODULE

PROGRAM FunctionResult03
      USE Mod
      IMPLICIT NONE

      CLASS(*), POINTER :: u_poly

      ALLOCATE( NextGen(4,1,4,2,5,10) :: u_poly )
      SELECT TYPE ( s => func(u_poly) )
        CLASS IS (NextGen(4,*,4,*,*,*))
           IF ( s%k1       .NE.   4 ) ERROR STOP 20
           IF ( s%l1       .NE.   1 ) ERROR STOP 21
           IF ( LEN(s%C0)  .NE.   1 ) ERROR STOP 22
           IF ( s%C0       .NE. '0' ) ERROR STOP 23
           IF ( SIZE(s%F0) .NE.   1 ) ERROR STOP 24
           IF ( ANY(s%F0 .NEQV. .True.) ) ERROR STOP 25
           IF ( s%k2       .NE.   4 ) ERROR STOP 26
           IF ( s%l2       .NE.   2 ) ERROR STOP 27
           IF ( SIZE(s%A0) .NE.   2 ) ERROR STOP 28
           IF ( ANY(s%A0   .NE. -1) ) ERROR STOP 29
           IF ( s%l3       .NE.   5 ) ERROR STOP 30
           IF ( s%l4       .NE.  10 ) ERROR STOP 31
           IF ( .NOT. ASSOCIATED(s%ptr) ) ALLOCATE( s%ptr, SOURCE = func( Base(4,3) ('IBM', .False.) ) )
             SELECT TYPE ( p => s%ptr )
                CLASS IS (Base(4,*))
                   IF ( p%k1       .NE.     4 ) ERROR STOP 40
                   IF ( p%l1       .NE.     3 ) ERROR STOP 41
                   IF ( LEN(p%C0)  .NE.     3 ) ERROR STOP 42
                   IF ( p%C0       .NE. 'IBM' ) ERROR STOP 23
                   IF ( SIZE(p%F0) .NE.     3 ) ERROR STOP 44
                   IF ( ANY(p%F0 .NEQV. .False.) ) ERROR STOP 45

                CLASS DEFAULT
                   STOP 46
             END SELECT

        CLASS DEFAULT
           STOP 32
      END SELECT

END PROGRAM FunctionResult03
