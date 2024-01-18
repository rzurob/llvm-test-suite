! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/F2003/initExp/Def/InitExpDefNULL.f
! opt variations: -qck -qnok -qnol

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefNULL.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Mar 29, 2006
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
!*  a reference to an tranformational intrinsic
!* 
!*  - NULL 
!*  (319366)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM  InitExpDefNULL 
  IMPLICIT NONE
  INTEGER :: I, J, K

  TYPE :: DTI(N1,K1,K2,K3,K4)    ! (20,1,2,4,8)
    INTEGER, KIND            :: K1,K2,K3,K4
    INTEGER, LEN             :: N1
    INTEGER(K1), ALLOCATABLE :: I1
    INTEGER(K2), POINTER     :: I2 => NULL()
    INTEGER(K3), ALLOCATABLE :: I4
    INTEGER(K4), POINTER     :: I8 => NULL()
  END TYPE

  TYPE(DTI(20,1,2,4,8)), PARAMETER :: T1=DTI(20,1,2,4,8)(I1=NULL(), I4=NULL())

  TYPE(DTI(20,1,2,4,8)) :: TI=DTI(20,1,2,4,8)(NULL(T1%I1), NULL(T1%I2), NULL(T1%I4), NULL(T1%I8) )

  TYPE :: DTL(N2,K5,K6,K7,K8)    ! (20,1,2,4,8)
    INTEGER, KIND            :: K5,K6,K7,K8
    INTEGER, LEN             :: N2
    LOGICAL(K5), ALLOCATABLE :: L1
    LOGICAL(K6), POINTER     :: L2 => NULL()
    LOGICAL(K7), ALLOCATABLE :: L4
    LOGICAL(K8), POINTER     :: L8 => NULL()
  END TYPE

  TYPE(DTL(20,1,2,4,8)), PARAMETER :: T2=DTL(20,1,2,4,8)(L1=NULL(), L4=NULL())

  TYPE(DTL(20,1,2,4,8)) :: TL=DTL(20,1,2,4,8)(NULL(T2%L1), NULL(T2%L2), NULL(T2%L4), NULL(T2%L8) )

  TYPE :: DTR(N3,K9,K10,K11)    ! (20,4,8,16)
    INTEGER, KIND          :: K9,K10,K11
    INTEGER, LEN           :: N3
    REAL(K9), ALLOCATABLE  :: R4
    REAL(K10), POINTER     :: R8 => NULL()
    REAL(K11),ALLOCATABLE  :: R16
  END TYPE

  TYPE(DTR(20,4,8,16)), PARAMETER :: T3=DTR(20,4,8,16)(R4=NULL(), R16=NULL())

  TYPE(DTR(20,4,8,16)) :: TR=DTR(20,4,8,16)(R4=NULL(T3%R4), R16=NULL(T3%R16) )

  TYPE :: DTZ(N4,K12,K13,K14)    ! (20,4,8,16)
    INTEGER, KIND             :: K12,K13,K14
    INTEGER, LEN              :: N4
    COMPLEX(K12), ALLOCATABLE :: Z4
    COMPLEX(K13), POINTER     :: Z8 => NULL()
    COMPLEX(K14),ALLOCATABLE  :: Z16
  END TYPE

  TYPE(DTZ(20,4,8,16)), PARAMETER :: T4=DTZ(20,4,8,16)(Z4=NULL(), Z16=NULL())

  TYPE(DTZ(20,4,8,16)) :: TZ=DTZ(20,4,8,16)(Z4=NULL(T4%Z4), Z16=NULL(T4%Z16) )

  TYPE :: DTC(K15,N5,N6)    ! (4,4,8)
    INTEGER, KIND              :: K15
    INTEGER, LEN               :: N5,N6
    CHARACTER(N5), ALLOCATABLE :: C4
    CHARACTER(N6), POINTER     :: C8 => NULL()
  END TYPE

  TYPE(DTC(4,4,8)), PARAMETER :: T5=DTC(4,4,8)(C4=NULL())

  TYPE(DTC(4,4,8)) :: TC=DTC(4,4,8)(C4=NULL(T5%C4), C8=NULL(T5%C8) )


  IF ( ALLOCATED (TI%I1) )       STOP 11
  IF ( ASSOCIATED(TI%I2) )       STOP 12
  IF ( ALLOCATED (TI%I4) )       STOP 13
  IF ( ASSOCIATED(TI%I8) )       STOP 14

  IF ( ALLOCATED (TL%L1) )       STOP 21
  IF ( ASSOCIATED(TL%L2) )       STOP 22
  IF ( ALLOCATED (TL%L4) )       STOP 23
  IF ( ASSOCIATED(TL%L8) )       STOP 24

  IF ( ALLOCATED (TR%R4) )       STOP 31
  IF ( ASSOCIATED(TR%R8) )       STOP 32
  IF ( ALLOCATED (TR%R16))       STOP 33

  IF ( ALLOCATED (TZ%Z4) )       STOP 41
  IF ( ASSOCIATED(TZ%Z8) )       STOP 42
  IF ( ALLOCATED (TZ%Z16))       STOP 43

  IF ( ALLOCATED (TC%C4) )       STOP 51
  IF ( ASSOCIATED(TC%C8) )       STOP 52

  END
