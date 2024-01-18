!*  ===================================================================
!*
!*                               DTP - Generic Type-Bound
!*
!*  DATE                       : October 02, 2008
!*
!*  PRIMARY SUBROUTINES TESTED   : Generic Resolution - Derived-type parameters
!*  SECONDARY SUBROUTINES TESTED : distinguish by name using NOPASS
!*                                 rank incompatible
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : GENERIC
!*
!*  DESCRIPTION                :
!*
!*  R448 type-bound-procedure-part is contains-stmt
!*                                     [ binding-private-stmt ]
!*                                     proc-binding-stmt
!*                                     [ proc-binding-stmt ] ...
!*
!*  R450 proc-binding-stmt is specific-binding
!*                         or generic-binding
!*                         or final-binding
!*
!*  R451 specific-binding is PROCEDURE [ (interface-name) ] &
!*                                    & [ [, binding-attr -list ] :: ] &
!*                                    & binding-name [ => procedure-name ]
!*
!*  R452 generic-binding is GENERIC [, access-spec ] :: generic-spec => binding-name-list
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      MODULE Mod1
      IMPLICIT NONE

      TYPE Base  (k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN :: l1

        CONTAINS
         PROCEDURE, NOPASS :: sub1
         PROCEDURE, NOPASS :: sub2
         GENERIC :: SUB =>  sub2, sub1
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (k2)
        INTEGER, KIND :: k2
      END TYPE Child

      TYPE, EXTENDS(Child) :: NextGen(k3)
        INTEGER, KIND :: k3
      END TYPE NextGen

      CHARACTER(10) :: tag

      CONTAINS
!*
      SUBROUTINE sub1(Arg0,Arg1,Arg2,Arg3)
      CLASS(Base(4,*)) :: Arg0, Arg2, Arg1(:), Arg3(:)

      IF (Arg0%k1 .NE. Arg2%k1) STOP 10
      IF (Arg1%k1 .NE. Arg3%k1) STOP 11

      tag ="1"

      END SUBROUTINE sub1

      SUBROUTINE sub2(Arg1,Arg0,Arg3,Arg2)
      CLASS(Base(4,*)) :: Arg0, Arg3 , Arg1(:), Arg2(:)

      IF (Arg0%k1 .NE. Arg3%k1) STOP 12
      IF (Arg1%k1 .NE. Arg2%k1) STOP 13

      tag ="2"

      END SUBROUTINE sub2

      END MODULE Mod1
!*
      PROGRAM Generic_TypeBound06b
      USE MOD1
      IMPLICIT NONE

      CLASS(Base(4,10)), POINTER :: b41 , b42
      CLASS(Base(4,:)), POINTER :: b81(:), b82(:)

      ALLOCATE(Base(4,20):: b81(10), b82(1) )

! two last arguments are TKR compatible but the two last are kind distinguishable

      call b41%sub(b41, b81, b42, b82)  !call to sub1
      IF ( tag .NE. "1" ) STOP 20
      call b81%sub(b41, b81, b42, b82)  !call to sub1
      IF ( tag .NE. "1" ) STOP 21

      call b41%sub(b81, b41, b42, b82)  !call to sub2
      IF ( tag .NE. "2" ) STOP 22
      call b81%sub(b81, b41, b42, b82)  !call to sub2
      IF ( tag .NE. "2" ) STOP 23

!using keywords : 2 first argument TKR compatible, second are distinguished by name

      call b41%sub(Arg1=b81, Arg0=b41, Arg2=b42, Arg3=b82)  !call to sub1
      IF ( tag .NE. "1" ) STOP 24
      call b81%sub(Arg1=b81, Arg0=b41, Arg2=b42, Arg3=b82)  !call to sub1
      IF ( tag .NE. "1" ) STOP 25

      call b41%sub(Arg1=b81, Arg0=b41, Arg3=b42, Arg2=b82)  !call to sub2
      IF ( tag .NE. "2" ) STOP 26
      call b81%sub(Arg1=b81, Arg0=b41, Arg3=b42, Arg2=b82)  !call to sub2
      IF ( tag .NE. "2" ) STOP 27

      END PROGRAM Generic_TypeBound06b
