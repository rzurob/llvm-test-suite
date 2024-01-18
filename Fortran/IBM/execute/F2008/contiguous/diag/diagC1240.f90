! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-11-25
!*
!*  PRIMARY FUNCTIONS TESTED   : CONTIGUOUS Attribute
!*  SECONDARY FUNCTIONS TESTED : -
!*                               -
!*
!*  DESCRIPTION                : -
!*                               -
!*  C1240:
!*   If an actual argument is an array pointer that has the ASYNCHRONOUS or VOLATILE
!*   attribute but does not have the CONTIGUOUS attribute, and the corresponding dummy argument has
!*   either the VOLATILE or ASYNCHRONOUS attribute, that dummy argument shall be an array pointer
!*   or an assumed-shape array that does not have the CONTIGUOUS attribute.
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
MODULE Mod
      IMPLICIT NONE

      CONTAINS

      SUBROUTINE sub_volatile(Arg)
        REAL(8), VOLATILE, CONTIGUOUS :: Arg(:,:,:)

      END SUBROUTINE sub_volatile

      SUBROUTINE sub_volatile_ptr(Arg)
        REAL(8), POINTER, VOLATILE, CONTIGUOUS :: Arg(:,:,:)

      END SUBROUTINE sub_volatile_ptr

      SUBROUTINE sub_volatile_tgt(Arg)
        REAL(8), TARGET, VOLATILE, CONTIGUOUS :: Arg(:,:,:)

      END SUBROUTINE sub_volatile_tgt

      SUBROUTINE sub_asyn(Arg)
        REAL(8), ASYNCHRONOUS, CONTIGUOUS :: Arg(:,:,:)

      END SUBROUTINE sub_asyn

      SUBROUTINE sub_asyn_ptr(Arg)
        REAL(8), POINTER, ASYNCHRONOUS, CONTIGUOUS :: Arg(:,:,:)

      END SUBROUTINE sub_asyn_ptr

      SUBROUTINE sub_asyn_tgt(Arg)
        REAL(8), TARGET, ASYNCHRONOUS, CONTIGUOUS :: Arg(:,:,:)

      END SUBROUTINE sub_asyn_tgt

END MODULE
PROGRAM diagC1240
      USE Mod
      IMPLICIT NONE

      REAL(8), POINTER, VOLATILE :: pV(:,:,:)
      REAL(8), POINTER, VOLATILE, CONTIGUOUS :: pcV(:,:,:)
      REAL(8), POINTER, ASYNCHRONOUS :: pA(:,:,:)
      REAL(8), POINTER, ASYNCHRONOUS, CONTIGUOUS :: pcA(:,:,:)

      CALL sub_volatile(pcV)                 !<--- valid
      CALL sub_volatile(pcV(:,:,:))          !<--- valid
      CALL sub_volatile(pcV(:,:,1:100))      !<--- valid
      CALL sub_volatile_ptr(pcV)             !<--- valid
      CALL sub_volatile_tgt(pcV)             !<--- valid

      CALL sub_volatile(pV)                  !<--- invalid
      CALL sub_volatile_ptr(pV)              !<--- invalid
      CALL sub_volatile_tgt(pV)              !<--- invalid

!*

      CALL sub_asyn(pcA)                     !<--- valid
      CALL sub_asyn(pcA(:,:,:))              !<--- valid
      CALL sub_asyn(pcA(:,:,:100))           !<--- valid
      CALL sub_asyn_ptr(pcA)                 !<--- valid
      CALL sub_asyn_tgt(pcA)                 !<--- valid

      CALL sub_asyn(pA)                      !<--- invalid
      CALL sub_asyn_ptr(pA)                  !<--- invalid
      CALL sub_asyn_tgt(pA)                  !<--- invalid

END PROGRAM diagC1240
