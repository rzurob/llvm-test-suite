!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Apr. 12, 2006
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
!*  a reference to an elemental intrinsic
!*
!*  -  MERGE
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

  TYPE :: DT0
    INTEGER(1)  :: I1(128)=(/(1,0, I=1,128,2)/)
    INTEGER(2)  :: I2(128)=(/(1,0, I=1,128,2)/)
    INTEGER(4)  :: I4(128)=(/(1,0, I=1,128,2)/)
    INTEGER(8)  :: I8(128)=(/(1,0, I=1,128,2)/)
    REAL(4)     :: R4(128)=(/(1.,0., I=1,128,2)/)
    REAL(8)     :: R8(128)=(/(1.,0., I=1,128,2)/)
    REAL(16)    :: R6(128)=(/(1.,0., I=1,128,2)/)
    LOGICAL(1)  :: L1(128)=(/(.TRUE., .FALSE., I=1,128,2)/)
    LOGICAL(2)  :: L2(128)=(/(.TRUE., .FALSE., I=1,128,2)/)
    LOGICAL(4)  :: L4(128)=(/(.TRUE., .FALSE., I=1,128,2)/)
    LOGICAL(8)  :: L8(128)=(/(.TRUE., .FALSE., I=1,128,2)/)
    COMPLEX(4)  :: Z4(128)=(/((1.,0.),(0.,1.), I=1,128,2)/)
    COMPLEX(8)  :: Z8(128)=(/((1.,0.),(0.,1.), I=1,128,2)/)
    COMPLEX(16) :: Z6(128)=(/((1.,0.),(0.,1.), I=1,128,2)/)
    CHARACTER   :: C1(128)=(/(ACHAR(1), ACHAR(0), I=1,128,2)/)
    CHARACTER   :: C2(128)=(/(ACHAR(0), ACHAR(1), I=1,128,2)/)
  END TYPE

  INTEGER, PARAMETER  :: II(128)=(/(0,1, I=1,128,2)/)
  REAL,    PARAMETER  :: RR(128)=(/(0.,1., I=1,128,2)/)
  LOGICAL, PARAMETER  :: LL(128)=(/(.FALSE.,.TRUE., I=1,128,2)/)
  COMPLEX, PARAMETER  :: ZZ(128)=(/((0.,1.),(1.,0.), I=1,128,2)/)

  TYPE(DT0), PARAMETER :: T=DT0()

  END MODULE


  PROGRAM  InitExpDefElemMERGE
  USE M, DT0=>DT0, I=>II, R=>RR, L=>LL, Z=>ZZ, T=>T
  IMPLICIT NONE
  INTEGER ::  J

  TYPE :: DT
    INTEGER(KIND(T%I1))  :: I1(128)
    INTEGER(KIND(T%I2))  :: I2(128)
    INTEGER(KIND(T%I4))  :: I4(128)
    INTEGER(KIND(T%I8))  :: I8(128)
    REAL(KIND(T%R4))     :: R4(128)
    REAL(KIND(T%R8))     :: R8(128)
    REAL(KIND(T%R6))     :: R6(128)
    LOGICAL(KIND(T%L1))  :: L1(128)
    LOGICAL(KIND(T%L2))  :: L2(128)
    LOGICAL(KIND(T%L4))  :: L4(128)
    LOGICAL(KIND(T%L8))  :: L8(128)
    COMPLEX(KIND(T%Z4))  :: Z4(128)
    COMPLEX(KIND(T%Z8))  :: Z8(128)
    COMPLEX(KIND(T%Z6))  :: Z6(128)
    CHARACTER(LEN(T%C1)) :: C (128)
  END TYPE

  LOGICAL,    PARAMETER  :: MASK(128)=(/(.TRUE.,.FALSE., J=1,128,2)/)

  TYPE (DT) :: T1=DT(                                                                 &
                        I1=MERGE(TSOURCE=T%I1, FSOURCE=INT(I,1), MASK=MASK),          &
                        I2=MERGE(TSOURCE=T%I2, FSOURCE=INT(I,2), MASK=MASK),          &
                        I4=MERGE(TSOURCE=T%I4, FSOURCE=INT(I,4), MASK=MASK),          &
                        I8=MERGE(TSOURCE=T%I8, FSOURCE=INT(I,8), MASK=MASK),          &
                        R4=MERGE(TSOURCE=T%R4, FSOURCE=REAL(R, 4), MASK=MASK),        &
                        R8=MERGE(TSOURCE=T%R8, FSOURCE=REAL(R, 8), MASK=MASK),        &
                        R6=MERGE(TSOURCE=T%R6, FSOURCE=REAL(R,16), MASK=MASK),        &
                        L1=MERGE(TSOURCE=T%L1, FSOURCE=LOGICAL(L,1), MASK=MASK),      &
                        L2=MERGE(TSOURCE=T%L2, FSOURCE=LOGICAL(L,2), MASK=MASK),      &
                        L4=MERGE(TSOURCE=T%L4, FSOURCE=LOGICAL(L,4), MASK=MASK),      &
                        L8=MERGE(TSOURCE=T%L8, FSOURCE=LOGICAL(L,8), MASK=MASK),      &
                        Z4=MERGE(TSOURCE=T%Z4, FSOURCE=CMPLX(Z,KIND=4), MASK=MASK),   &
                        Z8=MERGE(TSOURCE=T%Z8, FSOURCE=CMPLX(Z,KIND=8), MASK=MASK),   &
                        Z6=MERGE(TSOURCE=T%Z6, FSOURCE=CMPLX(Z,KIND=16), MASK=MASK),  &
                        C =MERGE(TSOURCE=T%C1, FSOURCE=T%C2, MASK=MASK)               &
                      )



  IF ( KIND(T1%I1)   .NE.  1 ) ERROR STOP 11
  IF ( KIND(T1%I2)   .NE.  2 ) ERROR STOP 12
  IF ( KIND(T1%I4)   .NE.  4 ) ERROR STOP 13
  IF ( KIND(T1%I8)   .NE.  8 ) ERROR STOP 14

  IF ( KIND(T1%R4)   .NE.  4 ) ERROR STOP 21
  IF ( KIND(T1%R8)   .NE.  8 ) ERROR STOP 22
  IF ( KIND(T1%R6)   .NE.  16) ERROR STOP 23

  IF ( KIND(T1%L1)   .NE.  1 ) ERROR STOP 31
  IF ( KIND(T1%L2)   .NE.  2 ) ERROR STOP 32
  IF ( KIND(T1%L4)   .NE.  4 ) ERROR STOP 33
  IF ( KIND(T1%L8)   .NE.  8 ) ERROR STOP 34

  IF ( KIND(T1%Z4)   .NE.  4 ) ERROR STOP 41
  IF ( KIND(T1%Z8)   .NE.  8 ) ERROR STOP 42
  IF ( KIND(T1%Z6)   .NE.  16) ERROR STOP 43

  IF ( ANY (T1%I1    .NE. 1 )) ERROR STOP 51
  IF ( ANY (T1%I2    .NE. 1 )) ERROR STOP 52
  IF ( ANY (T1%I4    .NE. 1 )) ERROR STOP 53
  IF ( ANY (T1%I8    .NE. 1 )) ERROR STOP 54

  IF ( ANY (T1%R4    .NE. 1 )) ERROR STOP 61
  IF ( ANY (T1%R8    .NE. 1 )) ERROR STOP 62
  IF ( ANY (T1%R6    .NE. 1 )) ERROR STOP 63

  IF ( ANY (T1%L1    .NEQV. .TRUE. )) ERROR STOP 71
  IF ( ANY (T1%L2    .NEQV. .TRUE. )) ERROR STOP 72
  IF ( ANY (T1%L4    .NEQV. .TRUE. )) ERROR STOP 73
  IF ( ANY (T1%L8    .NEQV. .TRUE. )) ERROR STOP 74

  IF ( ANY (T1%Z4    .NE. (1.,0.) )) ERROR STOP 81
  IF ( ANY (T1%Z8    .NE. (1.,0.) )) ERROR STOP 82
  IF ( ANY (T1%Z6    .NE. (1.,0.) )) ERROR STOP 83

  IF ( ANY (T1%C     .NE. ACHAR(1) )) ERROR STOP 99

  END

