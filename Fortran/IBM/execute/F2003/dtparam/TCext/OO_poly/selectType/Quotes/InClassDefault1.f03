! GB DTP extension using:
! ftcx_dtp -qk -ql -qreuse=none /tstdev/OO_poly/selectType/Quotes/InClassDefault1.f
! opt variations: -qck -qnok -qnol -qreuse=self

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 25, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Selector
!*
!*  REFERENCE                  : Feature 219934.OO_poly
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Within the CLASS DEFAULT, the associating entity is polymorphic
!*  and has the same declared type as the selector
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

    TYPE  :: Zero(N1,K1)    ! (20,1)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      INTEGER(K1)   :: Int=8_1
    END TYPE

    TYPE, EXTENDS(Zero)  :: Base(K2,K3,N2)    ! (20,1,8,8,1025)
      INTEGER, KIND :: K2,K3
      INTEGER, LEN  :: N2
      COMPLEX(K2)   :: Cplx=(-8.0_8, 8.0_8)
      LOGICAL(K3)   :: L=.true._8
      CHARACTER(N2) :: C
    END TYPE

    TYPE, EXTENDS(Base) :: Child(K4,N3)    ! (20,1,8,8,1025,4,20)
        INTEGER, KIND :: K4
        INTEGER, LEN  :: N3
    END TYPE

  END MODULE


  PROGRAM InClassDefault1
  USE M
  IMPLICIT NONE
  TYPE(Base(20,1,8,8,1025)) :: V(2:3,3:4)
  CHARACTER(1025) :: Str

  Str(:) = '!'
  V%C(:) = Str
  CALL Sub(V(2:3,3:4))

  CONTAINS

  SUBROUTINE Sub(Arg)
  CLASS(*) :: Arg(2:3,3:4)
  TYPE(Base(20,1,8,8,1025)) :: T
  INTEGER :: i

    SELECT TYPE (U => Arg)
    CLASS DEFAULT
      SELECT TYPE (U)
      CLASS IS (Base(*,1,8,8,*))
        T%L = .FALSE.
        T = U(2,3)
        IF ( .NOT. T%L )            ERROR STOP 20
        IF ( LEN(U%C) .NE. 1025 )   ERROR STOP 21
        IF ( ANY(U%C  .NE. Str) )   ERROR STOP 22

        IF ( .NOT. SAME_TYPE_AS(U, Arg))       ERROR STOP 30
        IF ( SIZE(U)          .NE. 4 )          ERROR STOP 31
        IF ( ANY (LBOUND(U)   .NE. (/2, 3/) ) ) ERROR STOP 32
        IF ( ANY (UBOUND(U)   .NE. (/3, 4/) ) ) ERROR STOP 33
        IF ( ANY(SHAPE(U)     .NE. (/2,2/)) )   ERROR STOP 34

        IF ( ANY(U%Int        .NE. 8_1) )      ERROR STOP 35
        IF ( KIND(U%Int) .NE. 1 )              ERROR STOP 36

        IF ( ANY(U%Cplx       .NE. (-8.0_8, 8.0_8) ))   ERROR STOP 37
        IF ( KIND(U%Cplx) .NE. 8 )                      ERROR STOP 38

        IF ( ANY(U%L        .NEQV. .TRUE._8) )   ERROR STOP 40
        IF ( KIND(U%L) .NE. 8 )                  ERROR STOP 41

        IF ( ANY(U%C    .NE. Str) )   ERROR STOP 42
        IF ( LEN(U%C) .NE. 1025 )     ERROR STOP 43

      CLASS DEFAULT
        STOP 51
      TYPE IS (Child(*,1,8,8,*,4,*))
        STOP 44
      END SELECT

    END SELECT

  END SUBROUTINE

  END



