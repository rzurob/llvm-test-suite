!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : ExplicitInitExp06.f
!*
!*  PROGRAMMER                 : Dorra Bouchiha
!*  DATE                       : April 24, 2009
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Explicit Init Expression
!*  SECONDARY FUNCTIONS TESTED : Defined assignment 
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
!*
!*  Defect 355394.3
!*
!234567890123456789012345678901234567890123456789012345678901234567890
MODULE Mod 
      IMPLICIT NONE

      TYPE Base (k1,l1)
        INTEGER, KIND :: k1 
        INTEGER, LEN  :: l1

        INTEGER(k1)    :: A1(l1-1) = -1 
        CHARACTER*(l1) :: C1 = 'C1'

        CONTAINS
        PROCEDURE, PASS :: assgnA => assgnBase 
        GENERIC :: assignment(=) => assgnA
      END TYPE

      TYPE, EXTENDS(Base) :: Child (k2,l2)
        INTEGER, KIND :: k2 
        INTEGER, LEN  :: l2

        INTEGER(k1)         :: A2(-1:k2-l1+2) = -2 
        CHARACTER*(l1+2*l2) :: C2 = 'C2'
        TYPE(Base(k1,l1))   :: bcomp 

        CONTAINS
        PROCEDURE, PASS :: assgnB => assgnChild 
        GENERIC :: assignment(=) => assgnB
      END TYPE

      TYPE, EXTENDS(Child) :: NextGen (k3,l3)
        INTEGER, KIND :: k3 
        INTEGER, LEN  :: l3

        INTEGER(k1)    :: A3(k3/k2) = -3
        CHARACTER*(l3) :: C3= 'C3'
        CLASS(Base(k1,:)), ALLOCATABLE :: poly

        CONTAINS
        PROCEDURE, PASS :: assgnC => assgnNextGen
        GENERIC :: assignment(=) => assgnC
      END TYPE

      CONTAINS

     SUBROUTINE assgnBase(this, arg)
       CLASS(Base(4,*)), INTENT(OUT) :: this 
       TYPE(Base(4,*)), INTENT(IN) :: arg 

       this%A1 = arg%A1 
       this%C1 = arg%C1 
     END SUBROUTINE

     SUBROUTINE assgnChild(this, arg)
       CLASS(Child(4,*,100,*)), INTENT(OUT) :: this 
       TYPE(Child(4,*,100,*)), INTENT(IN) :: arg 

       this%A1 = arg%A1 
       this%C1 = arg%C1 
       this%A2 = arg%A2 
       this%C2 = arg%C2 
       this%bcomp%A1 = arg%bcomp%A1
       this%bcomp%C1 = arg%bcomp%C1
     END SUBROUTINE

     SUBROUTINE assgnNextGen(this, arg)
       CLASS(NextGen(4,*,64,*,128,*)), INTENT(OUT) :: this 
       TYPE(NextGen(4,*,64,*,128,*)), INTENT(IN) :: arg 

       this%A1 = arg%A1 
       this%C1 = arg%C1 
       this%A2 = arg%A2 
       this%C2 = arg%C2 
       this%bcomp%A1 = arg%bcomp%A1
       this%bcomp%C1 = arg%bcomp%C1
       this%A3 = arg%A3 
       this%C3 = arg%C3 
       IF ( ALLOCATED( arg%poly ) ) THEN 
            ALLOCATE( this%poly, source = arg%poly )
       ELSE 
            ALLOCATE( this%poly, source = Base(4,this%k1) () ) 
       END IF 
     END SUBROUTINE

END MODULE
PROGRAM ExplicitInitExp06
      USE Mod
      IMPLICIT NONE

      TYPE(Base(4,5)) :: b2, b1 = Base(4,5) ( 10, 'AAAAA' )
      TYPE(Child(4,3,100,100)) :: c2, c1 = Child(4,3,100,100) ( 20,   &
                'BBBBB', 30, 'XL-compiler', Base(4,3) (40, 'CCCCC') )
      TYPE(NextGen(4,10,64,64,128,128)) :: n2, n1 =                   &
                NextGen(4,10,64,64,128,128) ( 11, 'Heisenberg',       & 
                22, 'David Hilbert', Base(4,10) (33, 'Sommerfeld'),   &  
                44, 'Ferdinand von Lindemann', NULL() ) 

      b2 = b1 
      IF ( SIZE(b2%A1)     .NE.       4 ) STOP 10
      IF ( LBOUND(b2%A1,1) .NE.       1 ) STOP 11
      IF ( UBOUND(b2%A1,1) .NE.       4 ) STOP 12
      IF ( ANY(b2%A1       .NE.     10) ) STOP 13
      IF ( LEN(b2%C1)      .NE.       5 ) STOP 14
      IF ( TRIM(b2%C1)     .NE. 'AAAAA' ) STOP 15

      c2 = c1 
      IF ( SIZE(c2%A1)     .NE.     2 ) STOP 20
      IF ( LBOUND(c2%A1,1) .NE.     1 ) STOP 21
      IF ( UBOUND(c2%A1,1) .NE.     2 ) STOP 22
      IF ( ANY(c2%A1       .NE.   20) ) STOP 23
      IF ( LEN(c2%C1)      .NE.     3 ) STOP 24
      IF ( TRIM(c2%C1)     .NE. 'BBB' ) STOP 25
      IF ( SIZE(c2%A2)     .NE.   101 ) STOP 26
      IF ( LBOUND(c2%A2,1) .NE.    -1 ) STOP 27
      IF ( UBOUND(c2%A2,1) .NE.    99 ) STOP 28
      IF ( ANY(c2%A2       .NE.   30) ) STOP 29
      IF ( LEN(c2%C2)      .NE.   203 ) STOP 30
      IF ( TRIM(c2%C2)   .NE. 'XL-compiler' ) STOP 31
      IF ( SIZE(c2%bcomp%A1)     .NE.     2 ) STOP 32
      IF ( LBOUND(c2%bcomp%A1,1) .NE.     1 ) STOP 33
      IF ( UBOUND(c2%bcomp%A1,1) .NE.     2 ) STOP 34
      IF ( ANY(c2%bcomp%A1       .NE.   40) ) STOP 35
      IF ( LEN(c2%bcomp%C1)      .NE.     3 ) STOP 36
      IF ( TRIM(c2%bcomp%C1)     .NE. 'CCC' ) STOP 37
  
      ALLOCATE( n1%poly, SOURCE = Base(4,10) (55, 'Rubinowicz') )
      n2 = n1 
      IF ( SIZE(n2%A1)     .NE.            9 ) STOP 40
      IF ( LBOUND(n2%A1,1) .NE.            1 ) STOP 41
      IF ( UBOUND(n2%A1,1) .NE.            9 ) STOP 42
      IF ( ANY(n2%A1       .NE.          11) ) STOP 43
      IF ( LEN(n2%C1)      .NE.           10 ) STOP 44
      IF ( TRIM(n2%C1)     .NE. 'Heisenberg' ) STOP 45
      IF ( SIZE(n2%A2)     .NE.           58 ) STOP 46
      IF ( LBOUND(n2%A2,1) .NE.           -1 ) STOP 47
      IF ( UBOUND(n2%A2,1) .NE.           56 ) STOP 48
      IF ( ANY(n2%A2       .NE.          22) ) STOP 49
      IF ( LEN(n2%C2)      .NE.          138 ) STOP 50
      IF ( TRIM(n2%C2)  .NE. 'David Hilbert' ) STOP 51
      IF ( SIZE(n2%bcomp%A1)        .NE.     9 ) STOP 52
      IF ( LBOUND(n2%bcomp%A1,1)    .NE.     1 ) STOP 53
      IF ( UBOUND(n2%bcomp%A1,1)    .NE.     9 ) STOP 54
      IF ( ANY(n2%bcomp%A1          .NE.   33) ) STOP 55
      IF ( LEN(n2%bcomp%C1)         .NE.    10 ) STOP 56
      IF ( TRIM(n2%bcomp%C1) .NE. 'Sommerfeld' ) STOP 57
      IF ( SIZE(n2%A3)     .NE.            2 ) STOP 58
      IF ( LBOUND(n2%A3,1) .NE.            1 ) STOP 59
      IF ( UBOUND(n2%A3,1) .NE.            2 ) STOP 60
      IF ( ANY(n2%A3       .NE.          44) ) STOP 61
      IF ( LEN(n2%C3)      .NE.          128 ) STOP 62
      IF ( TRIM(n2%C3) .NE. 'Ferdinand von Lindemann' ) STOP 63
      IF ( SIZE(n2%poly%A1)        .NE.     9 ) STOP 64
      IF ( LBOUND(n2%poly%A1,1)    .NE.     1 ) STOP 65
      IF ( UBOUND(n2%poly%A1,1)    .NE.     9 ) STOP 66
      IF ( ANY(n2%poly%A1          .NE.   55) ) STOP 67
      IF ( LEN(n2%poly%C1)         .NE.    10 ) STOP 68
      IF ( TRIM(n2%poly%C1) .NE. 'Rubinowicz' ) STOP 69
  
END PROGRAM ExplicitInitExp06
