! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/initExp/Def/InitExpDefPriorSpec.f
! opt variations: -ql

!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Apr. 06, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Fortran 2003 Initialization Expression Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 289074
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  If an initialization expression includes a specification inquiry that depends on a type
!*  parameter or an array bound of an entity specified in the same specification part,
!*  the type parameter or array bound must be specified in a prior specification of
!*  the specification part.
!*
!*  (319396)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

  TYPE :: DT(K1,K2,K3,K4)    ! (8,1,16,4)
    INTEGER, KIND :: K1,K2,K3,K4
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
  CLASS(DT(8,1,16,4)) :: Arg
  END SUBROUTINE

  END MODULE


  PROGRAM   InitExpDefPriorSpec
  USE M
  IMPLICIT NONE
  INTEGER :: I, J, K


  TYPE(DT(8,1,16,4)),    PARAMETER :: T(2147483645:2147483647)=DT(8,1,16,4)()
  INTEGER,     PARAMETER :: SIZ=3
  INTEGER,     PARAMETER :: LB1=2147483645
  INTEGER,     PARAMETER :: LB2=2147483646
  INTEGER,     PARAMETER :: LB3=2147483647

  CALL IntSub(T(LB1)%I8, T(LB2)%L1, T(LB3)%R6, T(LB3)%Z4, T)

  CONTAINS

  SUBROUTINE IntSub(TI8, TL1, TR6, TZ4, TD)

  INTEGER(KIND(T(LB1)%I8(::2))),  OPTIONAL, INTENT(IN) :: TI8(LBOUND(T(LB2)%I8,1):UBOUND(T(LB3)%I8,1))
  LOGICAL(KIND(T(LB1)%L1(::2))),  OPTIONAL, INTENT(IN) :: TL1(LBOUND(T(LB2)%L1,1):UBOUND(T(LB3)%L1,1))
  REAL(KIND(T(LB1)%R6(::2))),     OPTIONAL, INTENT(IN) :: TR6(LBOUND(T(LB2)%R6,1):UBOUND(T(LB3)%R6,1))
  COMPLEX(KIND(T(LB1)%Z4(::2))),  OPTIONAL, INTENT(IN) :: TZ4(LBOUND(T(LB2)%Z4,1):UBOUND(T(LB3)%Z4,1))
  CLASS(DT(8,1,16,4)),            OPTIONAL, INTENT(IN) :: TD (LBOUND(T ,1)     :UBOUND(T,1))


  IF ( KIND(TI8 )       .NE. 8 )                     ERROR STOP 11
  IF ( LBOUND(TI8,1 )   .NE. LBOUND(T(LB2)%I8,1) )   ERROR STOP 12
  IF ( UBOUND(TI8,1 )   .NE. UBOUND(T(LB3)%I8,1) )   ERROR STOP 13
  IF ( ANY( TI8         .NE. -1 ))                   ERROR STOP 14

  IF ( KIND(TL1 )       .NE. 1 )                     ERROR STOP 21
  IF ( LBOUND(TL1,1 )   .NE. LBOUND(T(LB2)%L1,1) )   ERROR STOP 22
  IF ( UBOUND(TL1,1 )   .NE. UBOUND(T(LB3)%L1,1) )   ERROR STOP 23
  IF ( ANY( TL1         .NEQV. T(LB2)%L1))           ERROR STOP 24

  IF ( KIND(TR6 )       .NE. 16)                     ERROR STOP 21
  IF ( LBOUND(TR6,1 )   .NE. LBOUND(T(LB2)%R6,1) )   ERROR STOP 22
  IF ( UBOUND(TR6,1 )   .NE. UBOUND(T(LB3)%R6,1) )   ERROR STOP 23
  IF ( ANY( TR6         .NE. T(LB3)%R6))             ERROR STOP 24

  IF ( KIND(TZ4 )       .NE. 4 )                     ERROR STOP 31
  IF ( LBOUND(TZ4,1 )   .NE. LBOUND(T(LB2)%Z4,1) )   ERROR STOP 32
  IF ( UBOUND(TZ4,1 )   .NE. UBOUND(T(LB3)%Z4,1) )   ERROR STOP 33
  IF ( ANY( TZ4         .NE. T(LB3)%Z4))             ERROR STOP 34

  IF ( LBOUND(TD,1 )    .NE. LBOUND(T,1) )           ERROR STOP 42
  IF ( UBOUND(TD,1 )    .NE. UBOUND(T,1) )           ERROR STOP 43
  IF ( ANY( TD(LB1)%Z4  .NE. T(LB1)%Z4))             ERROR STOP 44


  END SUBROUTINE

  END



