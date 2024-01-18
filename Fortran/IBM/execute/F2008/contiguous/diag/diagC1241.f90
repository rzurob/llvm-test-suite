! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : diagC1241.f
!*
!*  PROGRAMMER                 : Dorra Bouchiha 
!*  DATE                       : 2010-11-25
!*  ORIGIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : CONTIGUOUS Attribute   
!*                             :
!*  SECONDARY FUNCTIONS TESTED : - 
!*                               - 
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : - 
!*                               - 
!*  C1241:
!*        The actual argument corresponding to a dummy pointer with the CONTIGUOUS attribute 
!*        shall be simply contiguous. 
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

      SUBROUTINE sub(Arg)
        INTEGER, POINTER, CONTIGUOUS :: Arg(:,:,:)

      END SUBROUTINE sub
END MODULE
PROGRAM diagC1241
      USE Mod
      IMPLICIT NONE

      INTEGER, TARGET     :: T3(100,100,100)
      INTEGER, POINTER    :: P3(:,:,:)
      INTEGER, POINTER, CONTIGUOUS :: C3(:,:,:)

      P3 => T3                      !<--- not simply contiguous: invalid
      CALL sub(P3)                  

      P3 => T3(:,:,:)               !<--- not simply contiguous: invalid
      CALL sub(P3)                  

      P3 => T3(:,:,:100)            !<--- not simply contiguous: invalid
      CALL sub(P3)                  

      P3 => T3(:,:,1:100)           !<--- not simply contiguous: invalid
      CALL sub(P3)                  

      P3 => T3(:,:,1:100:1)         !<--- not simply contiguous: invalid
      CALL sub(P3)                  

      P3 => T3(:,1:100,:)           !<--- not simply contiguous: invalid
      CALL sub(P3)                  

      P3 => T3(:,:100,:)            !<--- not simply contiguous: invalid
      CALL sub(P3)                  

      P3 => T3(1:100,1:100,1:100)   !<--- not simply contiguous: invalid
      CALL sub(P3)                  

      C3 => T3                      !<--- simply contiguous: valid
      CALL sub(C3)                  

      C3 => T3(:,:,:)               !<--- simply contiguous: valid
      CALL sub(C3)                  

      C3 => T3(:,:,:100)            !<--- simply contiguous: valid
      CALL sub(C3)                  

      C3 => T3(:,:,1:100)           !<--- simply contiguous: valid
      CALL sub(C3)                  

      C3 => T3(:,:,1:100:1)         !<--- simply contiguous: valid
      CALL sub(C3)                  

      C3 => T3(:,1:100,:)           !<--- simply contiguous: valid
      CALL sub(C3)                 

      C3 => T3(:,:100,:)            !<--- simply contiguous: valid
      CALL sub(C3)                  

      C3 => T3(1:100,1:100,1:100)   !<--- simply contiguous: valid
      CALL sub(C3)                  

END PROGRAM diagC1241
