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
!*  C1239:
!*        If an actual argument is a nonpointer array that
!*        has the ASYNCHRONOUS or VOLATILE attribute but is not simply
!*        contiguous (6.5.4), and the corresponding dummy argument has
!*        either the VOLATILE or ASYNCHRONOUS attribute, that dummy
!*        argument shall be an assumed-shape array that does not have
!*        the CONTIGUOUS attribute.
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
        INTEGER, VOLATILE, CONTIGUOUS :: Arg(:,:,:)
      END SUBROUTINE sub_volatile

      SUBROUTINE sub_asyn(Arg)
        INTEGER, ASYNCHRONOUS, CONTIGUOUS :: Arg(:,:,:)
      END SUBROUTINE sub_asyn

END MODULE
PROGRAM diagC1239
      USE Mod
      IMPLICIT NONE

      INTEGER :: I, J
      INTEGER, PARAMETER :: N = 1, M = 100
      INTEGER, VOLATILE :: I3V(100,100,100)
      INTEGER, ASYNCHRONOUS :: I3A(100,100,100)

      CALL sub_volatile(I3V(:,:,:))                 !<--- simply contiguous: valid
      CALL sub_volatile(I3V(:,:,:100))              !<--- simply contiguous: valid
      CALL sub_volatile(I3V(:,:,1:100))             !<--- simply contiguous: valid

!  The XLF compiler is able to determine contiguity at compile time for these cases:
      CALL sub_volatile(I3V(:,1:100,:))             !<--- valid
      CALL sub_volatile(I3V(:,:,1:100:1))           !<--- valid
      CALL sub_volatile(I3V(1:100,1:100,1:100))     !<--- valid
      CALL sub_volatile(I3V(:,:100,:))              !<--- valid

      CALL sub_volatile(I3V(:,LBOUND(I3V, 1):UBOUND(I3V, 1),:))   !<--- valid
      CALL sub_volatile(I3V(:,N:M,:))                             !<--- valid

!  The XLF compiler is not able to determine contiguity at compile time for these cases:
      I = LBOUND(I3V, 1); J = UBOUND(I3V, 1)
      CALL sub_volatile(I3V(:,I:J,:))               !<--- invalid
      CALL sub_volatile(I3V(:,I:J:1,:))             !<--- invalid
      CALL sub_volatile(I3V(:,I+1:J-1:1,:))         !<--- invalid
!*
      CALL sub_asyn(I3A(:,:,:))                     !<--- simply contiguous: valid
      CALL sub_asyn(I3A(:,:,:100))                  !<--- simply contiguous: valid
      CALL sub_asyn(I3A(:,:,1:100))                 !<--- simply contiguous: valid

!  The XLF compiler is able to determine contiguity at compile time for these cases:
      CALL sub_asyn(I3A(:,1:100,:))                 !<--- valid
      CALL sub_asyn(I3A(:,:,1:100:1))               !<--- valid
      CALL sub_asyn(I3A(1:100,1:100,1:100))         !<--- valid
      CALL sub_asyn(I3A(:,:100,:))                  !<--- valid

      CALL sub_asyn(I3A(:,LBOUND(I3V, 1):UBOUND(I3A, 1),:))   !<--- valid
      CALL sub_asyn(I3A(:,N:M,:))                             !<--- valid

!  The XLF compiler is not able to determine contiguity at compile time for these cases:
      I = LBOUND(I3A, 1); J = UBOUND(I3A, 1)
      CALL sub_asyn(I3A(:,I:J,:))                   !<--- invalid
      CALL sub_asyn(I3A(:,I:J:1,:))                 !<--- invalid
      CALL sub_asyn(I3A(:,I+1:J-1:1,:))             !<--- invalid

END PROGRAM diagC1239
