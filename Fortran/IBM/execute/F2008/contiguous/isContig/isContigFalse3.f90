! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : isContigFalse3.f
!*
!*  PROGRAMMER                 : Dorra Bouchiha 
!*  DATE                       : 2010-10-25
!*  ORIGIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : IS_CONTIGUOUS intrinsic 
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : - 
!*                      
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
        CHARACTER(10) :: C0 
      END TYPE DT0

      TYPE DT1 (k1,l1)
        INTEGER, KIND :: k1 = 4
        INTEGER, LEN  :: l1 = 10

        INTEGER(k1) :: I1 
        CHARACTER(k1) :: C1
      END TYPE DT1

      TYPE DT2 (l1)
        INTEGER, LEN  :: l1 = 10    

        CHARACTER(l1) :: C2
      END TYPE DT2
END MODULE Mod1
PROGRAM isContigFalse3
      USE Mod1
      IMPLICIT NONE

      TYPE(DT0) :: dtv0(10)
      TYPE(DT1) :: dtv1(10)
      TYPE(DT2) :: dtv2(10)

      IF ( .NOT. IS_CONTIGUOUS(dtv0) )   ERROR STOP 10
      IF ( .NOT. IS_CONTIGUOUS(dtv1) )   ERROR STOP 11
      IF ( .NOT. IS_CONTIGUOUS(dtv2) )   ERROR STOP 12

      IF ( .NOT. IS_CONTIGUOUS(dtv0%C0) )   ERROR STOP 13
      IF ( IS_CONTIGUOUS(dtv1%C1) )   ERROR STOP 14
      IF ( IS_CONTIGUOUS(dtv1%I1) )   ERROR STOP 15
      IF ( .NOT. IS_CONTIGUOUS(dtv2%C2) )   ERROR STOP 16

END PROGRAM isContigFalse3

