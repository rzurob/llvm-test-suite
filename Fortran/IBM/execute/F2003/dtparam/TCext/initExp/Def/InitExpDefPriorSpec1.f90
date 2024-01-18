! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/initExp/Def/InitExpDefPriorSpec1.f
! opt variations: -qnol

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefPriorSpec1.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Apr. 06, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Fortran 2003 Initialization Expression Enhancement 
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature Number 289074 
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  
!*  The prior specification may be to the left of the specification inquiry in the same 
!*  statement, but must not be within the same entity declaration.
!* 
!* 
!*  (319209/319718)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

  TYPE :: DT(N1,K1,K2,K3,K4)    ! (20,8,1,16,4)
    INTEGER, KIND :: K1,K2,K3,K4
    INTEGER, LEN  :: N1
    INTEGER(K1)   :: I8(-2147483648:-2147483646) = -1
    LOGICAL(K2)   :: L1(-128:-126) = (/.TRUE., .TRUE.,.TRUE./) 
    REAL(K3)      :: R6(125:127) = (/(-1.0 , I=1,3)/)
    COMPLEX(K4)   :: Z4(-32768:-32766) = (1.0, -1.0)
    PROCEDURE(CHARACTER), POINTER, NOPASS :: ProcPtr=>NULL()
    CONTAINS
    PROCEDURE  :: Proc => ModSub
  END TYPE

  CONTAINS

  SUBROUTINE ModSub(Arg)
  CLASS(DT(*,8,1,16,4)) :: Arg
  END SUBROUTINE

  END MODULE


  PROGRAM   InitExpDefPriorSpec1 
  USE M
  IMPLICIT NONE
  INTEGER :: I, J, K

   
  TYPE(DT(20,8,1,16,4)),    PARAMETER :: T(2147483645:2147483647)=DT(20,8,1,16,4)()
  INTEGER,     PARAMETER :: SIZ=3
  

  INTEGER(KIND(T(1)%I8(:))), PARAMETER :: TI8(LBOUND(T(2)%I8,1):UBOUND(T(3)%I8,1))=1,            &
                                         TTI8(LBOUND(TI8,1):UBOUND(TI8,1))=SIZE(TI8)
  LOGICAL(KIND(T(1)%L1(:))), PARAMETER :: TL1(LBOUND(T(2)%L1,1):UBOUND(T(3)%L1,1))=.TRUE.,       &
                                         TTL1(LBOUND(TL1,1):UBOUND(TL1,1))=.TRUE. 
  REAL(KIND(T(1)%R6(:))),    PARAMETER :: TR6(LBOUND(T(2)%R6,1):UBOUND(T(3)%R6,1))=1.0,          &
                                         TTR6(LBOUND(TR6,1):UBOUND(TR6,1))=SIZE(TR6) 
  COMPLEX(KIND(T(1)%Z4(:))), PARAMETER :: TZ4(LBOUND(T(2)%Z4,1):UBOUND(T(3)%Z4,1))=(1.0,-1.0),   &
                                         TTZ4(LBOUND(TZ4,1):UBOUND(TZ4,1))=SIZE(TZ4)
  TYPE(DT(20,8,1,16,4)),     PARAMETER :: TD(LBOUND(T ,1):UBOUND(T,1))=DT(20,8,1,16,4)(),                    &
                                          TTD(LBOUND(TD ,1):UBOUND(TD,1))=DT(20,8,1,16,4)(1)
 
 
  IF ( KIND(TTI8 )       .NE. 8 )                   STOP 11
  IF ( LBOUND(TTI8,1 )   .NE. LBOUND(TI8,1) )       STOP 12
  IF ( UBOUND(TTI8,1 )   .NE. UBOUND(TI8,1) )       STOP 13
  IF ( ANY( TTI8         .NE. SIZE(TI8)))           STOP 14

  IF ( KIND(TTL1 )       .NE. 1 )                   STOP 21
  IF ( LBOUND(TTL1,1 )   .NE. LBOUND(TL1,1) )       STOP 22
  IF ( UBOUND(TTL1,1 )   .NE. UBOUND(TL1,1) )       STOP 23
  IF ( ANY( TTL1         .NEQV. .TRUE.))            STOP 24

  IF ( KIND(TTR6 )       .NE. 16)                   STOP 21
  IF ( LBOUND(TTR6,1 )   .NE. LBOUND(TR6,1) )       STOP 22
  IF ( UBOUND(TTR6,1 )   .NE. UBOUND(TR6,1) )       STOP 23
  IF ( ANY( TTR6         .NE. SIZE(TR6)))           STOP 24

  IF ( KIND(TTZ4 )       .NE. 4 )                   STOP 31
  IF ( LBOUND(TTZ4,1 )   .NE. LBOUND(TZ4,1) )       STOP 32
  IF ( UBOUND(TTZ4,1 )   .NE. UBOUND(TZ4,1) )       STOP 33
  IF ( ANY( TTZ4         .NE. SIZE(TZ4)))           STOP 34

  IF ( LBOUND(TTD,1 )    .NE. LBOUND(T,1) )         STOP 42
  IF ( UBOUND(TTD,1 )    .NE. UBOUND(T,1) )         STOP 43
  IF ( ANY( TTD(LBOUND(TD,1))%Z4    .NE. (1.0, -1.0)))         STOP 44
  IF ( ANY( TTD(UBOUND(TD,1))%Z4    .NE. (1.0, -1.0)))         STOP 54


  END


 
