!*  ===================================================================
!*
!*  DATE                       : April 24, 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : Array constructor with Type Specification
!*  SECONDARY FUNCTIONS TESTED : Function Result + Select Type
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  Testing the use of Brackets ('[]') for the Array Constructor:
!*  Testing the usage of an Array Constructor as the Actual Argument of a FUNCTION
!*
!234567890123456789012345678901234567890123456789012345678901234567890
MODULE Mod
      IMPLICIT NONE

      TYPE Base (k1,l1)
         INTEGER, KIND :: k1
         INTEGER, LEN  :: l1

         INTEGER(k1) :: A1(l1)
      END TYPE

      TYPE, EXTENDS(Base) :: Child (k2,l2)
         INTEGER, KIND :: k2
         INTEGER, LEN  :: l2

         TYPE(Base(k2,l2)), ALLOCATABLE :: poly(:)
      END TYPE

      CONTAINS

      FUNCTION CreateNew(arg) Result(Res)
       CLASS(Base(4,*)), INTENT(IN) :: arg(:)
       CLASS(Base(4,:)), ALLOCATABLE :: Res(:)

       ALLOCATE( Res(SIZE(arg)), SOURCE = arg )

      END FUNCTION
END MODULE
PROGRAM DTP_ACE_15
      USE Mod

      SELECT TYPE ( s => CreateNew( [ Base(4,5) :: Base(4,5)(5) ] ) )
         TYPEIS (Base(4,*))
           IF ( s%k1       .NE.  4 ) ERROR STOP 11
           IF ( s%l1       .NE.  5 ) ERROR STOP 12
           IF ( SIZE(s)    .NE.  1 ) ERROR STOP 13
           DO I = 1, SIZE(s)
              IF ( SIZE(s(I)%A1) .NE.  5 ) ERROR STOP 14
              IF ( ANY(s(I)%A1   .NE. 5) ) ERROR STOP 15
           END DO

         TYPE IS (Child(4,*,4,*))
           STOP 16

        CLASS DEFAULT
           STOP 17
      END SELECT

      SELECT TYPE ( s => CreateNew([Base(4,10) ::  &
                        Base(4,10)([(I, I=1, 10)]), Base(4,10)([(2*I, I=1, 10)])]) )
         TYPEIS (Base(4,*))
           IF ( s%k1       .NE.  4 ) ERROR STOP 18
           IF ( s%l1       .NE. 10 ) ERROR STOP 19
           IF ( SIZE(s)    .NE.  2 ) ERROR STOP 20
           DO I = 1, SIZE(s)
              IF ( SIZE(s(I)%A1) .NE. 10) ERROR STOP 21
              IF ( ANY(s(I)%A1   .NE. [(I*J, J=1, 10)]) ) ERROR STOP 22
           END DO

         TYPE IS (Child(4,*,4,*))
           STOP 23

        CLASS DEFAULT
           STOP 24
      END SELECT

      SELECT TYPE ( s =>  CreateNew( (/Base(4,3) :: Base(4,3)( [1,2,3] )/) ) )
         TYPEIS (Base(4,*))
           IF ( s%k1       .NE.  4 ) ERROR STOP 25
           IF ( s%l1       .NE.  3 ) ERROR STOP 26
           IF ( SIZE(s)    .NE.  1 ) ERROR STOP 27
           DO I = 1, SIZE(s)
              IF ( SIZE(s(I)%A1) .NE.  3 ) ERROR STOP 28
              IF ( ANY(s(I)%A1   .NE. [1,2,3]) ) ERROR STOP 29
           END DO

         TYPE IS (Child(4,*,4,*))
           STOP 30

        CLASS DEFAULT
           STOP 31
      END SELECT

      SELECT TYPE ( s =>  CreateNew( [  Child(4,1,4,1) ::   &
                     Child(4,1,4,1) ([99], [Base(4,1) :: Base(4,1)(98)]) ] ) )
         TYPE IS (Child(4,*,4,*))
           IF ( s%k1    .NE.  4 ) ERROR STOP 32
           IF ( s%l1    .NE.  1 ) ERROR STOP 33
           IF ( s%k2    .NE.  4 ) ERROR STOP 34
           IF ( s%l2    .NE.  1 ) ERROR STOP 35
           IF ( SIZE(s) .NE.  1 ) ERROR STOP 36
           DO I = 1, SIZE(s)
              IF ( SIZE(s(I)%A1)  .NE.   1 ) ERROR STOP 37
              IF ( ANY(s(I)%A1    .NE. 99) ) ERROR STOP 38
              IF ( .NOT. ALLOCATED(s(I)%poly) ) ERROR STOP 39
              IF ( s(I)%poly%k1       .NE.  4 ) ERROR STOP 40
              IF ( s(I)%poly%l1       .NE.  1 ) ERROR STOP 41
              IF ( SIZE(s(I)%poly)    .NE.  1 ) ERROR STOP 42
                 DO J = 1, SIZE(s(I)%poly)
                    IF ( SIZE(s(I)%poly(J)%A1) .NE.     1 ) ERROR STOP 43
                    IF ( ANY(s(I)%poly(J)%A1   .NE. [98]) ) ERROR STOP 44
                 END DO
            END DO

        CLASS DEFAULT
           STOP 45
      END SELECT
END PROGRAM DTP_ACE_15