!*  ===================================================================
!*
!*  DATE                       : August 13, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : SELECT TYPE Construct - Derived-type parameters
!*  SECONDARY FUNCTIONS TESTED : SELECT CASE nested in SELECT TYPE
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
        INTEGER, KIND :: k1
        INTEGER, LEN :: len1

        CHARACTER(LEN=len1):: tag = 'Sum'
      END TYPE Basic

      TYPE, EXTENDS(Basic) :: ExtBasic (k2)
        INTEGER, KIND :: k2

        REAL(k2), ALLOCATABLE :: my_arr(:)
      END TYPE ExtBasic

      INTEGER, PARAMETER :: k1 = KIND(0), k2 = KIND(0.0) , len1 = 5
      INTEGER :: I

    END MODULE Mod1

    PROGRAM Select_Type_Basic10
      USE Mod1
      IMPLICIT NONE

      INTERFACE
         SUBROUTINE Sub1(p)
           USE Mod1
           CLASS(*) :: p
         END SUBROUTINE Sub1
      END INTERFACE

      TYPE(ExtBasic(k1,len1,k2)), TARGET :: Indv
      CLASS(Basic(k1,len1)), POINTER :: Pnt

      ALLOCATE(Indv%my_arr(2*len1))
      IF ( .NOT. ALLOCATED(Indv%my_arr)) STOP 10

      Indv%my_arr = (/ (1.3, 2.7, I = 1, len1)/)

      CALL Sub1(Indv)

      Pnt => Indv
      IF ( .NOT. ASSOCIATED(Pnt)) STOP 11

      SELECT TYPE(A=>Pnt)

        CLASS IS (ExtBasic(k1,*,k2))
           A%tag = 'Sum'
           CALL Sub1(A)

        CLASS IS (Basic(k1,*))
          STOP 20

        CLASS DEFAULT
          STOP 21
      END SELECT

    END PROGRAM Select_Type_Basic10

    SUBROUTINE Sub1(T)
      USE Mod1
      IMPLICIT NONE

      CLASS(*) ::  T
      REAL :: tmp

      SELECT TYPE(A=>T)
        TYPE IS (ExtBasic(k1,*,k2))

           SELECT CASE (A%tag)
             CASE ('Sum')
                   tmp = SUM(A%my_arr)
                   print *, FLOOR(tmp)
             CASE ('Pdt')
                   tmp = PRODUCT(A%my_arr)
                   print *, FLOOR(tmp)
             CASE DEFAULT
                   STOP 40
           END SELECT

        CLASS DEFAULT
          STOP 30
      END SELECT

    END SUBROUTINE Sub1
