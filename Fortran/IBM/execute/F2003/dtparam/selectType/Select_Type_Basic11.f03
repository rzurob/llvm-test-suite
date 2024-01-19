!*  ===================================================================
!*
!*  DATE                       : August 13, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : SELECT TYPE Construct - Derived-type parameters
!*  SECONDARY FUNCTIONS TESTED : Argument Association / Use Asociation
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
!234567890123456789012345678901234567890123456789012345678901234567890
    MODULE Mod1
      IMPLICIT NONE

      TYPE Basic (k1,len1)
        INTEGER, KIND :: k1 = 4
        INTEGER, LEN :: len1 = 10
      END TYPE Basic

      TYPE, EXTENDS(Basic) :: ExtBasic
        REAL(k1), POINTER :: Ptr(:) => NULL()
      END TYPE ExtBasic

      INTEGER, PARAMETER :: k1 = KIND(0.0) , len1 = 10
      INTEGER :: I

    END MODULE Mod1

    PROGRAM Select_Type_Basic11
      USE Mod1
      IMPLICIT NONE

      INTERFACE
         SUBROUTINE Sub1(k,p)
           USE Mod1
           INTEGER, INTENT(IN) :: k
           REAL, ALLOCATABLE, TARGET :: t(:)
           CLASS(Basic(k1,len1)) :: p
         END SUBROUTINE Sub1
      END INTERFACE

      TYPE(ExtBasic(k1,len1)), TARGET :: Indv
      CLASS(Basic(k1,len1)), POINTER :: My_poly
      INTEGER :: M

      M = 10
      ALLOCATE(Indv%Ptr(M))

      My_poly => Indv
      IF ( .NOT. ASSOCIATED(My_poly)) ERROR STOP 11

      SELECT TYPE(A=>My_poly)

        CLASS IS (ExtBasic(k1,*))
           CALL Sub1(M,A)

        CLASS IS (Basic(k1,*))
          STOP 12

        CLASS DEFAULT
          STOP 13
      END SELECT

    END PROGRAM Select_Type_Basic11

    SUBROUTINE Sub1(N,T)
      USE Mod1
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: N
      REAL, ALLOCATABLE, TARGET :: tgt(:)
      CLASS(Basic(k1,len1)) :: T

      tgt = (/ (1., I = 1, N)/)

      SELECT TYPE(A=>T)
        TYPE IS (ExtBasic(k1,*))
           A%Ptr => tgt
           IF ( SIZE(A%Ptr) .NE. N) ERROR STOP 20
           IF ( INT(SUM(A%Ptr(:))) .NE. N) ERROR STOP 21

        CLASS DEFAULT
          STOP 22
      END SELECT

    END SUBROUTINE Sub1
