!*  ===================================================================
!*
!*                               DTP-SELECT TYPE Construct
!*
!*  DATE                       : August 12, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : SELECT TYPE Construct - Derived-type parameters
!*  SECONDARY FUNCTIONS TESTED : type-bound procedure - 'recursive' type
!*                               Use association
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

      TYPE Base  (k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN :: l1

        INTEGER(k1) :: position = 0
      END TYPE Base

      TYPE, EXTENDS(Base) :: List
        TYPE(List(k1,l1)), POINTER :: Next => NULL()
        CONTAINS
        PROCEDURE :: GetSum => GET_CUMM_SUM
      END TYPE List

      INTEGER, PARAMETER :: knd1 = KIND(0), len1 = 10

      CONTAINS

      SUBROUTINE GET_CUMM_SUM (OBJ)
        CLASS(List(knd1,*)), TARGET  :: OBJ

       print*, OBJ%position, CUMM_SUM(OBJ%position)

      END SUBROUTINE GET_CUMM_SUM

      SUBROUTINE BuildList(T)
        CLASS(List(knd1,len1)), POINTER :: T, Node1, Node2
        INTEGER :: I

       Node1 => T

       DO I = 1, 10
         ALLOCATE(Node2)
         Node2%position = I
         Node1%Next => Node2
         Node1 => Node2
       END DO

      END SUBROUTINE BuildList
!*
      RECURSIVE FUNCTION CUMM_SUM (N)  result(answer)
        INTEGER :: answer
        INTEGER, INTENT(IN) :: N

      IF (N <= 0) THEN
        answer = 1
      ELSE
        answer = CUMM_SUM (N-1)+ CUMM_SUM (N-3)
      END IF

      END FUNCTION CUMM_SUM

      END MODULE Mod1

      PROGRAM Select_Type06a
      USE Mod1
      IMPLICIT NONE

      CLASS(*), POINTER :: DTV
      CLASS(List(knd1,len1)), POINTER :: TEMP, INIT
      INTEGER :: I

      ALLOCATE(List(knd1,len1):: Init)
      IF ( .NOT. ASSOCIATED(Init)) STOP 10

      CALL BuildList(Init)
      DTV => Init

      SELECT TYPE ( DTV )
        TYPE IS (List(knd1,*))

          TEMP => DTV

          DO I = 1, 10
           IF ( .NOT. ASSOCIATED(TEMP%NEXT) ) STOP 20
           TEMP => TEMP%NEXT
           Call TEMP%GetSum()
          END DO

        TYPE IS (Base(knd1,*))
           STOP 21

        CLASS DEFAULT
           STOP 22

      END SELECT

      END PROGRAM Select_Type06a
