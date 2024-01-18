! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : isContigTrue2.f
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

      CONTAINS

      SUBROUTINE Sub1(Arg)
        INTEGER, CONTIGUOUS :: Arg(:)

        IF ( .NOT. IS_CONTIGUOUS(Arg) )          ERROR STOP 70
        IF ( .NOT. IS_CONTIGUOUS(Arg(5:)) )      ERROR STOP 71
        IF ( .NOT. IS_CONTIGUOUS(Arg(1:0)) )     ERROR STOP 72
        IF ( .NOT. IS_CONTIGUOUS(Arg(4:9)) )     ERROR STOP 73
        IF ( .NOT. IS_CONTIGUOUS(Arg(2:10:1)) )  ERROR STOP 74
      END SUBROUTINE Sub1
      SUBROUTINE Sub5(Arg)
        INTEGER, CONTIGUOUS :: Arg(:,:,:,:,:)

        IF ( .NOT. IS_CONTIGUOUS(Arg) )                     ERROR STOP 80
        IF ( .NOT. IS_CONTIGUOUS(Arg(:,:,:,:,:)) )          ERROR STOP 81
        IF ( .NOT. IS_CONTIGUOUS(Arg(:,:,:,:,1:)) )         ERROR STOP 82
        IF ( .NOT. IS_CONTIGUOUS(Arg(:,:,:,:,1:2)) )        ERROR STOP 83
        IF ( .NOT. IS_CONTIGUOUS(Arg(:,:,:,:,1:2:1)) )      ERROR STOP 84
        IF ( .NOT. IS_CONTIGUOUS(Arg(1:,1:,1:,1:,:)) )      ERROR STOP 85
        IF ( .NOT. IS_CONTIGUOUS(Arg(1:,1:,1:,1:,1:)) )     ERROR STOP 86
        IF ( .NOT. IS_CONTIGUOUS(Arg(1:2,1:2,1:2,1:2,:)) )  ERROR STOP 87
      END SUBROUTINE Sub5
END MODULE Mod1
PROGRAM isContigTrue2
      USE Mod1
      IMPLICIT NONE

      INTEGER  :: L, U, P, Q

      INTEGER :: I0D(0)
      INTEGER :: I1D(10)
      INTEGER :: I5D(2,2,2,2,2)
      INTEGER, ALLOCATABLE :: all(:)
      INTEGER, ALLOCATABLE :: all5D(:,:,:,:,:)

      IF ( .NOT. IS_CONTIGUOUS(I1D) )          ERROR STOP 10
      IF ( .NOT. IS_CONTIGUOUS(I1D(5:)) )      ERROR STOP 11
      IF ( .NOT. IS_CONTIGUOUS(I1D(1:0)) )     ERROR STOP 12
      IF ( .NOT. IS_CONTIGUOUS(I1D(4:9)) )     ERROR STOP 13
      IF ( .NOT. IS_CONTIGUOUS(I1D(2:10:1)) )  ERROR STOP 14

      L = LBOUND(I1D, 1)
      U = UBOUND(I1D, 1)

      IF ( .NOT. IS_CONTIGUOUS(I1D(L:U)) )          ERROR STOP 15
      IF ( .NOT. IS_CONTIGUOUS(I1D(L+1:)) )         ERROR STOP 16
      IF ( .NOT. IS_CONTIGUOUS(I1D(:U-1)) )         ERROR STOP 17
      IF ( .NOT. IS_CONTIGUOUS(I1D(L+3:U-3:1)) )    ERROR STOP 18
      IF ( .NOT. IS_CONTIGUOUS(I5D(1:2,1,1,1,1)) )  ERROR STOP 19

!**************** zero sized array and AC ***********************

      IF ( .NOT. IS_CONTIGUOUS(I0D) )                  ERROR STOP 20
      IF ( .NOT. IS_CONTIGUOUS(I1D(2:1:1)) )           ERROR STOP 21
      IF ( .NOT. IS_CONTIGUOUS(I5D(2:1:1,1,1,1,1)) )   ERROR STOP 22
      IF ( .NOT. IS_CONTIGUOUS(I5D(1,1,1,1,1:2:-1)) )  ERROR STOP 23

!******************** rank > 1 ******************************
      
      IF ( .NOT. IS_CONTIGUOUS(I5D) )                     ERROR STOP 24
      IF ( .NOT. IS_CONTIGUOUS(I5D(:,:,:,:,:)) )          ERROR STOP 25
      IF ( .NOT. IS_CONTIGUOUS(I5D(:,:,:,:,1:)) )         ERROR STOP 26
      IF ( .NOT. IS_CONTIGUOUS(I5D(:,:,:,:,1:2)) )        ERROR STOP 27
      IF ( .NOT. IS_CONTIGUOUS(I5D(:,:,:,:,1:2:1)) )      ERROR STOP 28
      IF ( .NOT. IS_CONTIGUOUS(I5D(1:,1:,1:,1:,:)) )      ERROR STOP 29
      IF ( .NOT. IS_CONTIGUOUS(I5D(1:,1:,1:,1:,1:)) )     ERROR STOP 30
      IF ( .NOT. IS_CONTIGUOUS(I5D(1:2,1:2,1:2,1:2,:)) )  ERROR STOP 31

      L = LBOUND(I5D, 5)
      U = UBOUND(I5D, 5)
      IF ( .NOT. IS_CONTIGUOUS(I5D(:,:,:,:,L:U)) )          ERROR STOP 32
      IF ( .NOT. IS_CONTIGUOUS(I5D(L:,L:,L:,L:,L:)) )       ERROR STOP 33
      IF ( .NOT. IS_CONTIGUOUS(I5D(L:U,L:U,L:U,:,:)) )      ERROR STOP 34
      IF ( .NOT. IS_CONTIGUOUS(I5D(L:U,L:U,L:U,L:U,L:U)) )  ERROR STOP 35
 
      CALL Sub1(I1D)
      CALL Sub5(I5D)

      ALLOCATE( all(10), all5D(2,2,2,2,2) ) 

      IF ( .NOT. IS_CONTIGUOUS(all) )          ERROR STOP 40
      IF ( .NOT. IS_CONTIGUOUS(all(5:)) )      ERROR STOP 41
      IF ( .NOT. IS_CONTIGUOUS(all(1:0)) )     ERROR STOP 42
      IF ( .NOT. IS_CONTIGUOUS(all(4:9)) )     ERROR STOP 43
      IF ( .NOT. IS_CONTIGUOUS(all(2:10:1)) )  ERROR STOP 44

      L = LBOUND(all, 1)
      U = UBOUND(all, 1)

      IF ( .NOT. IS_CONTIGUOUS(all(L:U)) )          ERROR STOP 45
      IF ( .NOT. IS_CONTIGUOUS(all(L+1:)) )         ERROR STOP 46
      IF ( .NOT. IS_CONTIGUOUS(all(:U-1)) )         ERROR STOP 47
      IF ( .NOT. IS_CONTIGUOUS(all(L+3:U-3:1)) )    ERROR STOP 48
      IF ( .NOT. IS_CONTIGUOUS(all5D(1:2,1,1,1,1)) )  ERROR STOP 49

!**************** zero sized array and AC ***********************

      IF ( .NOT. IS_CONTIGUOUS(all(2:1:1)) )           ERROR STOP 50
      IF ( .NOT. IS_CONTIGUOUS(all5D(2:1:1,1,1,1,1)) )   ERROR STOP 51
      IF ( .NOT. IS_CONTIGUOUS(all5D(1,1,1,1,1:2:-1)) )  ERROR STOP 52

!******************** rank > 1 ******************************

      IF ( .NOT. IS_CONTIGUOUS(all5D) )                     ERROR STOP 53
      IF ( .NOT. IS_CONTIGUOUS(all5D(:,:,:,:,:)) )          ERROR STOP 54
      IF ( .NOT. IS_CONTIGUOUS(all5D(:,:,:,:,1:)) )         ERROR STOP 55
      IF ( .NOT. IS_CONTIGUOUS(all5D(:,:,:,:,1:2)) )        ERROR STOP 56
      IF ( .NOT. IS_CONTIGUOUS(all5D(:,:,:,:,1:2:1)) )      ERROR STOP 57
      IF ( .NOT. IS_CONTIGUOUS(all5D(1:,1:,1:,1:,:)) )      ERROR STOP 58
      IF ( .NOT. IS_CONTIGUOUS(all5D(1:,1:,1:,1:,1:)) )     ERROR STOP 59
      IF ( .NOT. IS_CONTIGUOUS(all5D(1:2,1:2,1:2,1:2,:)) )  ERROR STOP 60

      L = LBOUND(I5D, 5)
      U = UBOUND(I5D, 5)
      IF ( .NOT. IS_CONTIGUOUS(all5D(:,:,:,:,L:U)) )          ERROR STOP 61
      IF ( .NOT. IS_CONTIGUOUS(all5D(L:,L:,L:,L:,L:)) )       ERROR STOP 62
      IF ( .NOT. IS_CONTIGUOUS(all5D(L:U,L:U,L:U,:,:)) )      ERROR STOP 63
      IF ( .NOT. IS_CONTIGUOUS(all5D(L:U,L:U,L:U,L:U,L:U)) )  ERROR STOP 64

      CALL Sub1(all)
      CALL Sub5(all5D)

END PROGRAM isContigTrue2
