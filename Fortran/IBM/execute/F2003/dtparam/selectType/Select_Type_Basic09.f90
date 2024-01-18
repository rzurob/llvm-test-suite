!*  ===================================================================
!*
!*  DATE                       : August 05, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : SELECT TYPE Construct - Derived-type parameters
!*  SECONDARY FUNCTIONS TESTED : Array Constructor
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
        INTEGER, KIND :: k1 = KIND(0.0)
        INTEGER, LEN :: len1 = 10

        CHARACTER(LEN=len1) :: name(4)=(/CHARACTER(10):: 'Niels ', 'Henrik', 'David ', 'Bohr  '/)
      END TYPE Basic

      TYPE, EXTENDS(Basic) :: ExtBasic
        CLASS(Basic(k1,:)), POINTER :: Ptr => NULL()
      END TYPE ExtBasic

      END MODULE Mod1

      PROGRAM Select_Type_Basic09
      USE Mod1
      IMPLICIT NONE

      INTEGER, PARAMETER :: k1 = KIND(0.0) , len1 = 10
      CLASS(ExtBasic(k1,len1)), POINTER :: Indv(:)

      ALLOCATE(ExtBasic(k1,len1)::Indv(4))
      IF ( .NOT. ASSOCIATED(Indv)) STOP 10

      CALL Sub1(Indv)

      CONTAINS

      SUBROUTINE Sub1(T)
      CLASS(*) ::  T(:)

      SELECT TYPE(A=>(/T/))
        CLASS IS (ExtBasic(4,*))
          print 999, TRIM(A(4)%name(1)), TRIM(A(4)%name(2)), TRIM(A(4)%name(3)), TRIM(A(4)%name(4))

        CLASS DEFAULT
          STOP 20
      END SELECT

999   FORMAT (1x, A7)

      END SUBROUTINE
!*
      END PROGRAM Select_Type_Basic09
