!*  ===================================================================
!*
!*                               DTP-SELECT TYPE Construct
!*
!*  DATE                       : August 26, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : SELECT TYPE Construct - Derived-type parameters
!*  SECONDARY FUNCTIONS TESTED : Use association
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : SELECT TYPE Construct
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*  8.1.5.1 Form of the SELECT TYPE construct
!*
!*  R821 select-type-construct  is  select-type-stmt
!*                                      [ type-guard-stmt
!*                                        block ] ...
!*                                      end-select-type-stmt
!*  R822 select-type-stmt       is  [ select-construct-name : ] SELECT TYPE&
!*                                      &( [ associate-name => ] selector )
!*
!*  R823 type-guard-stmt is TYPE IS ( type-spec ) [ select-construct-name ]
!*                       or CLASS IS ( type-spec ) [ select-construct-name ]
!*                       or CLASS DEFAULT [ select-construct-name ]
!*
!* See defect 355394
!*
!234567890123456789012345678901234567890123456789012345678901234567890
MODULE Mod1
      IMPLICIT NONE

      TYPE Base  (k1,l1)
        INTEGER, KIND :: k1 = KIND(0)
        INTEGER, LEN  :: l1 = 20

        INTEGER(k1) :: my_arr(l1)
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child
        CLASS(Base(k1,l1)), POINTER :: Cmp
      END TYPE Child

      INTEGER, PARAMETER :: knd1 = KIND(0), len1 = 10, sum_c = 55
      INTEGER :: I

      CONTAINS

      SUBROUTINE sub1(Obj)
         CLASS(Child(knd1,:)), ALLOCATABLE, INTENT(INOUT) :: Obj

         ALLOCATE( Child(knd1,len1):: Obj )
         IF ( .NOT. ALLOCATED(Obj) ) STOP 10

         Obj%my_arr = (/ (I, I = 1, len1) /)

         ASSOCIATE ( p => Obj%my_arr )
            IF ( SUM(p) .NE. sum_c ) STOP 11
            DO I = 1, len1
               IF ( p(I)   .NE.     I ) STOP 12
            END DO
         END ASSOCIATE

         CALL sub3(Obj)
      END SUBROUTINE sub1

      SUBROUTINE sub2(Obj)
         CLASS(Child(knd1,:)), ALLOCATABLE, INTENT(INOUT) :: Obj
         TYPE(Base(knd1,len1)), TARGET :: tgt = Base(knd1,len1) ( my_arr=(/ (1, I = 1, len1) /) ) ! has the SAVE attribute

         Obj%Cmp => tgt
         IF ( .NOT. ASSOCIATED(Obj%Cmp) ) STOP 20

         ASSOCIATE ( p => Obj%Cmp%my_arr )
            IF ( SUM(p) .NE. len1 ) STOP 21
            IF ( ANY(p  .NE. 1   )) STOP 22
         END ASSOCIATE
      END SUBROUTINE sub2

      SUBROUTINE sub3(Arg)
         CLASS(*) :: Arg

         SELECT TYPE ( A => Arg )
           TYPE IS (Child(knd1,*))
             ASSOCIATE ( p => A%my_arr )
               IF ( SUM(p) .NE. sum_c ) STOP 30
               DO I = 1, len1
                  IF ( p(I) .NE.    I ) STOP 31
               END DO
             END ASSOCIATE

           TYPE IS (Base(knd1,*))
              STOP 32

           CLASS IS (Child(knd1,*))
              STOP 33

           CLASS IS (Base(knd1,*))
              STOP 34

           CLASS DEFAULT
              STOP 35
         END SELECT
      END SUBROUTINE sub3

END MODULE Mod1
!*
PROGRAM Select_Type03a
      USE Mod1
      IMPLICIT NONE

      CLASS(Child(knd1,:)), ALLOCATABLE :: dtv

      CALL sub1(dtv)
      IF ( .NOT. ALLOCATED(dtv) ) STOP 101

      CALL sub2(dtv)
      IF ( .NOT. ASSOCIATED(dtv%Cmp) ) STOP 102

      SELECT TYPE ( A => dtv%Cmp)
        TYPE IS (Child(knd1,*))
           STOP 103

        CLASS IS (Child(knd1,*))
           STOP 104

        CLASS IS (Base(knd1,*))
           ASSOCIATE ( p => A%my_arr )
             IF ( SUM(p) .NE. len1 ) STOP 105
             IF ( ANY(p  .NE. 1   )) STOP 106
           END ASSOCIATE

        CLASS DEFAULT
           STOP 107
      END SELECT

END PROGRAM Select_Type03a
