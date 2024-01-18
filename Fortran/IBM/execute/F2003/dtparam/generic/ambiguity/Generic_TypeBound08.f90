!*  ===================================================================
!*
!*                               DTP - Generic Type-Bound
!*
!*  DATE                       : November 2, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : Generic Resolution - Derived-type parameters
!*  SECONDARY FUNCTIONS TESTED : Resolution for intrinsic function
!*                               based on type and kind
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
!*                                    & [ [ , binding-attr -list ] :: ] &
!*                                    & binding-name [ => procedure-name ]
!*
!*  R452 generic-binding is GENERIC [, access-spec ] :: generic-spec => binding-name-list
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      MODULE Mod1
      IMPLICIT NONE

      TYPE Base  (k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN  :: l1

        INTEGER(k1) :: base_arr(l1)

        CONTAINS
         PROCEDURE, PASS :: SIZE_OBJ4
         GENERIC :: SIZE =>  SIZE_OBJ4
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (k2,l2)
        INTEGER, KIND :: k2
        INTEGER, LEN  :: l2

        INTEGER(k1) :: child_arr(l1+l2)
      END TYPE Child

      CONTAINS

      INTEGER FUNCTION SIZE_OBJ4(Obj)
        CLASS(Base(4,*)) :: Obj
        INTEGER :: I

        SELECT TYPE ( Obj )
          TYPE IS (Child(4,*,4,*))
             Obj%base_arr  = (/ (550, I = 1, Obj%l1)/)
             Obj%child_arr = (/ (-550, I = 1, Obj%l2)/)

             size_obj4 = SIZE(Obj%base_arr) + SIZE(Obj%child_arr)

          TYPE IS (Base(4,*))
             Obj%base_arr  = (/ (5000, I = 1, Obj%l1)/)

             size_obj4 = SIZE(Obj%base_arr)

          CLASS DEFAULT
           STOP 111
      END SELECT

      END FUNCTION SIZE_OBJ4

      END MODULE Mod1
!*
      MODULE Mod2
      USE MOD1
      IMPLICIT NONE

      TYPE, EXTENDS(Child) :: NextGen(k3,l3,l4)
        INTEGER, KIND :: k3
        INTEGER, LEN  :: l3 , l4

        INTEGER(k1) :: nextg_arr(l1+l2+l3)

        CONTAINS
         PROCEDURE, PASS :: SIZE_OBJ8
         GENERIC :: SIZE =>  SIZE_OBJ8
      END TYPE NextGen

      CONTAINS

      INTEGER FUNCTION SIZE_OBJ8(Obj)
        CLASS(NextGen(8,*,4,*,4,*,*)) :: Obj
        INTEGER :: I

        Obj%base_arr  = (/ (1, I = 1, Obj%l1)/)
        Obj%child_arr = (/ (-10, I = 1, Obj%l2)/)
        Obj%nextg_arr =  (/ (10, I = 1, Obj%l3)/)

        size_obj8 = SIZE(Obj%base_arr) + SIZE(Obj%child_arr) + SIZE(Obj%nextg_arr)

      END FUNCTION SIZE_OBJ8

      END MODULE Mod2
!*
      PROGRAM Generic_TypeBound08
      USE MOD2
      IMPLICIT NONE

      CLASS(Base(4,:)), POINTER :: ptr1
      TYPE(Child(4,5,4,10)), TARGET :: tgt1
      TYPE(Base(4,5))  :: base1
      TYPE(NextGen(8,10,4,1,4,1,1)) :: dtv


      IF ( base1%size() .NE. size_obj4(base1) ) STOP 10
      IF ( base1%size() .NE. size(base1%base_arr) ) STOP 11

      ALLOCATE(Base(4,10):: ptr1)
      IF ( .NOT. ASSOCIATED(ptr1)) STOP 12

      SELECT TYPE (ptr1)
          TYPE IS (Base(4,*))
           IF (ptr1%size() .NE. size_obj4(ptr1)) STOP 13
           IF (ptr1%size() .NE. size(ptr1%base_arr)) STOP 14

          CLASS DEFAULT
           STOP 15
      END SELECT

      ptr1 => tgt1
      IF ( .NOT. ASSOCIATED(ptr1)) STOP 16

      SELECT TYPE (ptr1)
          TYPE IS (Child(4,*,4,*))
           IF (ptr1%size() .NE. size_obj4(ptr1)) STOP 17
           IF (ptr1%size() .NE. (size(ptr1%base_arr)+size(ptr1%child_arr)) )STOP 18

          CLASS DEFAULT
           STOP 19
      END SELECT

      IF ( dtv%size() .NE. size_obj8(dtv) ) STOP 20
      IF ( dtv%size() .NE. (size(dtv%base_arr)+size(dtv%child_arr)+size(dtv%nextg_arr)) ) STOP 21

      END PROGRAM Generic_TypeBound08
