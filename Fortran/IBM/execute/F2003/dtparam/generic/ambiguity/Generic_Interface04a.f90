!*  ===================================================================
!*
!*                               DTP - Generic Interface
!*
!*  DATE                       : October 02, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : Generic Resolution - Derived-type parameters
!*  SECONDARY FUNCTIONS TESTED : Resolution based on KIND type parameter
!*                               for polymorphic objects
!*                               IMPLICIT mapping
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : GENERIC
!*
!*  DESCRIPTION                :
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      MODULE Mod1
      IMPLICIT NONE

      TYPE Base (k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN :: l1

        INTEGER((k1-2)/2) :: data
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child1 (k2,l2)
        INTEGER, KIND :: k2
        INTEGER, LEN :: l2
      END TYPE Child1

      TYPE, EXTENDS(Base) :: Child2 (k3,l3)
        INTEGER, KIND :: k3
        INTEGER, LEN :: l3

        TYPE(Child1(k3-k1,l3,k1,l1)) :: dtv
      END TYPE Child2

      INTERFACE FUNC
         FUNCTION FUNC_kind10(obj)
           IMPORT BASE
           IMPLICIT CLASS(Base(10,*)) (o) ! kind=10 Ok as far as not used to intrinsic entities
           IMPLICIT CLASS(Base(10,:)) (f)
           POINTER :: FUNC_kind10
         END FUNCTION

         FUNCTION FUNC_kind18(obj)
           IMPORT BASE
           IMPLICIT CLASS(Base(18,*)) (o) ! kind=18 Ok as far as not used to intrinsic entities
           IMPLICIT CLASS(Base(18,:)) (f)
           POINTER :: FUNC_kind18
         END FUNCTION
      END INTERFACE

      END MODULE Mod1
!*
      FUNCTION FUNC_kind10(obj)
        USE MOD1, Only: Base
        IMPLICIT CLASS(Base(10,*)) (o)
        IMPLICIT CLASS(Base(10,:)) (f)
        POINTER :: FUNC_kind10

        ALLOCATE(FUNC_kind10, source=obj)
        IF ( .NOT. ASSOCIATED(FUNC_kind10)) STOP 100

        FUNC_kind10%data = obj%k1

      END FUNCTION

      FUNCTION FUNC_kind18(obj)
        USE MOD1, Only: Base
        IMPLICIT CLASS(Base(18,*)) (o)
        IMPLICIT CLASS(Base(18,:)) (f)
        POINTER :: FUNC_kind18

        ALLOCATE(FUNC_kind18, source=obj)
        IF ( .NOT. ASSOCIATED(FUNC_kind18)) STOP 101

        FUNC_kind18%data = obj%k1

      END FUNCTION
!*
      PROGRAM Generic_Interface04a
      USE MOD1
      IMPLICIT NONE

      TYPE(Child2(10,10,20,20)), TARGET :: c_simple
      TYPE(Child2(18,18,36,36)) :: c_double
      TYPE(Base(10,10))  :: b_simple
      TYPE(Base(18,18))  :: b_double

      CLASS(*), POINTER :: upoly
      CLASS(Base(10,:)), POINTER  :: bpoly_simple
      CLASS(Base(18,:)), POINTER  :: bpoly_double

      SELECT TYPE( A=> FUNC(b_double) )
        TYPE IS (Base(18,*))
          IF (A%data .NE. 18) STOP 10

        CLASS DEFAULT
          STOP 11
      END SELECT

      ASSOCIATE ( A=> FUNC(b_simple) )
          IF (A%data .NE. 10) STOP 12
      END ASSOCIATE

      upoly => FUNC(b_simple)

      SELECT TYPE( upoly )
        CLASS IS (Base(10,*))
          IF (upoly%data .NE. 10) STOP 13

        CLASS DEFAULT
          STOP 14
      END SELECT

      bpoly_simple => c_simple

      SELECT TYPE( A => FUNC(bpoly_simple) )
        CLASS IS (Base(10,*))
          IF (A%data .NE. 10) STOP 15

        CLASS DEFAULT
          STOP 16
      END SELECT

      SELECT TYPE( a => bpoly_simple )
        CLASS IS (Child2(10,*,20,*))
           ASSOCIATE ( b => FUNC(a%dtv) )
               IF (b%data .NE. 10) STOP 17
           END ASSOCIATE

        CLASS DEFAULT
          STOP 18
      END SELECT

      ALLOCATE(bpoly_double, source=c_double)

      ASSOCIATE ( A=> FUNC(bpoly_double) )
          IF (A%data .NE. 18) STOP 19
      END ASSOCIATE

      END PROGRAM Generic_Interface04a
