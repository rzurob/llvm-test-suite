!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : DTP_ACE_13.f
!*
!*  PROGRAMMER                 : Dorra Bouchiha
!*  DATE                       : April 24, 2009
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array constructor with Type Specification
!*  SECONDARY FUNCTIONS TESTED : Function Result 
!*
!*
!*  DRIVER STANZA              : xlf2003
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
!* Defect : 366211
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
        TYPE(Base(4,*)), INTENT(IN) :: arg(:)
        TYPE(Base(4,:)), ALLOCATABLE :: Res(:)

        ALLOCATE( Res(SIZE(arg)), SOURCE = arg ) 

      END FUNCTION  
END MODULE
PROGRAM DTP_ACE_13
      USE Mod 
      IMPLICIT NONE
      INTEGER :: I, J 
      TYPE(Base(4,:)), ALLOCATABLE :: b1(:)
      TYPE(Child(4,1,4,1)) :: c1 
      TYPE(Child(4,:,4,:)), ALLOCATABLE :: c2

      b1 = CreateNew( [ Base(4,5) :: Base(4,5)(5) ] )
      IF ( .NOT. ALLOCATED(b1) ) STOP 10
      IF ( b1%k1       .NE.  4 ) STOP 11
      IF ( b1%l1       .NE.  5 ) STOP 12
      IF ( SIZE(b1)    .NE.  1 ) STOP 13
      DO I = 1, SIZE(b1)
         IF ( SIZE(b1(I)%A1) .NE.  5 ) STOP 14
         IF ( ANY(b1(I)%A1   .NE. 5) ) STOP 15
      END DO 

      b1 = CreateNew( [Base(4,10) :: Base(4,10)([(I, I=1, 10)]), Base(4,10)([(2*I, I=1, 10)])] )
      IF ( .NOT. ALLOCATED(b1) ) STOP 14
      IF ( b1%k1       .NE.  4 ) STOP 15
      IF ( b1%l1       .NE. 10 ) STOP 16
      IF ( SIZE(b1)    .NE.  2 ) STOP 17
      DO I = 1, SIZE(b1)
         IF ( SIZE(b1(I)%A1) .NE. 10) STOP 18
         IF ( ANY(b1(I)%A1   .NE. [(I*J, J=1, 10)]) ) STOP 19
      END DO 

      b1 = CreateNew( (/Base(4,3) :: Base(4,3)( [1,2,3] )/) )
      IF ( .NOT. ALLOCATED(b1) ) STOP 20
      IF ( b1%k1       .NE.  4 ) STOP 21
      IF ( b1%l1       .NE.  3 ) STOP 22
      IF ( SIZE(b1)    .NE.  1 ) STOP 23
      DO I = 1, SIZE(b1)
         IF ( SIZE(b1(I)%A1) .NE.  3 ) STOP 24
         IF ( ANY(b1(I)%A1   .NE. [1,2,3]) ) STOP 25
      END DO 

      c1 = Child(4,1,4,1) ( [99], CreateNew([Base(4,1) :: Base(4,1)(98)]) ) 
      IF ( c1%k1    .NE.  4 ) STOP 27
      IF ( c1%l1    .NE.  1 ) STOP 27
      IF ( c1%k2    .NE.  4 ) STOP 28
      IF ( c1%l2    .NE.  1 ) STOP 29
      IF ( SIZE(c1%A1)  .NE.   1 ) STOP 30
      IF ( ANY(c1%A1    .NE. 99) ) STOP 31
      IF ( .NOT. ALLOCATED(c1%poly) ) STOP 32
      IF ( c1%poly%k1       .NE.  4 ) STOP 33
      IF ( c1%poly%l1       .NE.  1 ) STOP 34
      IF ( SIZE(c1%poly)    .NE.  1 ) STOP 35
      DO I = 1, SIZE(c1%poly)
         IF ( SIZE(c1%poly(I)%A1) .NE.     1 ) STOP 36
         IF ( ANY(c1%poly(I)%A1   .NE. [98]) ) STOP 37
      END DO 

      c2 = c1
      IF ( c2%k1    .NE.  4 ) STOP 38
      IF ( c2%l1    .NE.  1 ) STOP 39
      IF ( c2%k2    .NE.  4 ) STOP 40
      IF ( c2%l2    .NE.  1 ) STOP 41
      IF ( SIZE(c2%A1)  .NE.   1 ) STOP 42
      IF ( ANY(c2%A1    .NE. 99) ) STOP 43
      IF ( .NOT. ALLOCATED(c2%poly) ) STOP 44
      IF ( c2%poly%k1       .NE.  4 ) STOP 45
      IF ( c2%poly%l1       .NE.  1 ) STOP 46
      IF ( SIZE(c2%poly)    .NE.  1 ) STOP 47
      DO I = 1, SIZE(c2%poly)
         IF ( SIZE(c2%poly(I)%A1) .NE.     1 ) STOP 48
         IF ( ANY(c2%poly(I)%A1   .NE. [98]) ) STOP 49
      END DO 

      c2%poly = CreateNew([Base(4,1) :: Base(4,1)(97)])
      IF ( .NOT. ALLOCATED(c2%poly) ) STOP 50
      IF ( c2%poly%k1       .NE.  4 ) STOP 51
      IF ( c2%poly%l1       .NE.  1 ) STOP 52
      IF ( SIZE(c2%poly)    .NE.  1 ) STOP 53
      DO I = 1, SIZE(c2%poly)
         IF ( SIZE(c2%poly(I)%A1) .NE.   1 ) STOP 54
         IF ( ANY(c2%poly(I)%A1   .NE. 97) ) STOP 55
      END DO 
END PROGRAM DTP_ACE_13
