! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-10-25
!*
!*  PRIMARY FUNCTIONS TESTED   : IS_CONTIGUOUS intrinsic
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : -
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
MODULE Mod1
      IMPLICIT NONE

      TYPE DT0
        INTEGER :: I0
      END TYPE DT0

      TYPE DT1 (k1,l1)
        INTEGER, KIND :: k1 = 4
        INTEGER, LEN  :: l1 = 10

        INTEGER(k1) :: I1
      END TYPE DT1

      TYPE DT2
        CHARACTER(0) :: C2
      END TYPE DT2

      TYPE DT3 (k1)
        INTEGER, KIND :: k1 = 4

        CHARACTER(k1) :: C3
      END TYPE DT3

      TYPE DT4 (l1)
        INTEGER, LEN  :: l1 = 4

        CHARACTER(l1) :: C4
      END TYPE DT4

END MODULE Mod1
PROGRAM isContigTrue5
      USE Mod1
      IMPLICIT NONE

      TYPE(DT0) :: dtv0(10)
      TYPE(DT1) :: dtv1(10)
      TYPE(DT2) :: dtv2(10)
      TYPE(DT3) :: dtv3(10)
      TYPE(DT4) :: dtv4(10)

      IF ( .NOT. IS_CONTIGUOUS(dtv0) )     ERROR STOP 10
      IF ( .NOT. IS_CONTIGUOUS(dtv1) )     ERROR STOP 11
      IF ( .NOT. IS_CONTIGUOUS(dtv2) )     ERROR STOP 12
      IF ( .NOT. IS_CONTIGUOUS(dtv3) )     ERROR STOP 13
      IF ( .NOT. IS_CONTIGUOUS(dtv4) )     ERROR STOP 14

      IF ( .NOT. IS_CONTIGUOUS(dtv0%I0) )     ERROR STOP 20
      IF ( .NOT. IS_CONTIGUOUS(dtv1%I1) )     ERROR STOP 21
      IF ( .NOT. IS_CONTIGUOUS(dtv2%C2) )     ERROR STOP 22
      IF ( .NOT. IS_CONTIGUOUS(dtv3%C3) )     ERROR STOP 23
      IF ( .NOT. IS_CONTIGUOUS(dtv4%C4) )     ERROR STOP 24

END PROGRAM isContigTrue5
