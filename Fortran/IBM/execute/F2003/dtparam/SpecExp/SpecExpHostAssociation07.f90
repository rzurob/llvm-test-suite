!*  ===================================================================
!*
!*  DATE                       : June 14, 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : Specification expression - Host Association
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
!*  An objet designator with a base object that is made accessible by host association
!*
!234567890123456789012345678901234567890123456789012345678901234567890
PROGRAM SpeExpHostAssociation07
      IMPLICIT NONE

      TYPE Base (k1,l1)
         INTEGER, KIND :: k1 = 4
         INTEGER, LEN  :: l1 = 2

         INTEGER(k1) :: I1 = k1, A1(l1) = 2*k1
         CHARACTER(l1) :: Carr = 'B'
      END TYPE

      TYPE, EXTENDS(Base) :: Child (k2,l2)
         INTEGER, KIND :: k2 = 4
         INTEGER, LEN  :: l2 = 4

         TYPE(Base(k2,l2)) :: b1
      END TYPE

      CALL Sub11()
      CALL Sub21()
      CALL Sub31()

      CONTAINS

      SUBROUTINE Sub11()
        TYPE(Base(4,SIZE([Base(I1=-1),Base(I1=1)]))) :: Obj

         IF ( Obj%k1 .NE. 4 ) STOP 10
         IF ( Obj%l1 .NE. 2 ) STOP 11
         IF ( Obj%I1 .NE. 4 ) STOP 12
         IF ( SIZE(Obj%A1)   .NE.   2 ) STOP 13
         IF ( ANY(Obj%A1     .NE.  8) ) STOP 14
         IF ( LEN(Obj%Carr)  .NE.   2 ) STOP 15
         IF ( TRIM(Obj%Carr) .NE. 'B' ) STOP 16
      END SUBROUTINE Sub11

      SUBROUTINE Sub21()
        TYPE(Child(4*LBOUND([Base()],1),2*LBOUND([Base()],1),     &
                   8*UBOUND([Base()],1),3*UBOUND([Base()],1))) :: Obj

         IF ( Obj%k1 .NE. 4 ) STOP 10
         IF ( Obj%l1 .NE. 2 ) STOP 11
         IF ( Obj%k2 .NE. 8 ) STOP 10
         IF ( Obj%l2 .NE. 3 ) STOP 11
         IF ( Obj%I1 .NE. 4 ) STOP 12
         IF ( SIZE(Obj%A1)   .NE.   2 ) STOP 13
         IF ( ANY(Obj%A1     .NE.  8) ) STOP 14
         IF ( LEN(Obj%Carr)  .NE.   2 ) STOP 15
         IF ( TRIM(Obj%Carr) .NE. 'B' ) STOP 16

         IF ( Obj%b1%k1 .NE. 8 ) STOP 10
         IF ( Obj%b1%l1 .NE. 3 ) STOP 11
         IF ( Obj%b1%I1 .NE. 8 ) STOP 12
         IF ( SIZE(Obj%b1%A1)   .NE.   3 ) STOP 13
         IF ( ANY(Obj%b1%A1     .NE. 16) ) STOP 14
         IF ( LEN(Obj%b1%Carr)  .NE.   3 ) STOP 15
         IF ( TRIM(Obj%b1%Carr) .NE. 'B' ) STOP 16
      END SUBROUTINE Sub21

      SUBROUTINE Sub31()
        TYPE(Base(4,SIZE([Child(b1=Base(4,4)())]))) :: Obj

         IF ( Obj%k1 .NE. 4 ) STOP 17
         IF ( Obj%l1 .NE. 1 ) STOP 18
         IF ( Obj%I1 .NE. 4 ) STOP 19
         IF ( SIZE(Obj%A1)   .NE.   1 ) STOP 20
         IF ( ANY(Obj%A1     .NE.  8) ) STOP 21
         IF ( LEN(Obj%Carr)  .NE.   1 ) STOP 22
         IF ( TRIM(Obj%Carr) .NE. 'B' ) STOP 23
      END SUBROUTINE Sub31
END PROGRAM SpeExpHostAssociation07
