!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug. 22, 2006
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
!*  CVMGN -- an IBM extension
!*
!*  (324400)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

  !INTEGER(1),  PARAMETER  :: I1(128)=(/(1,0, I=1,128,2)/)
   INTEGER(2),  PARAMETER  :: I2(128)=(/(1,0, I=1,128,2)/)
   INTEGER(4),  PARAMETER  :: I4(128)=(/(1,0, I=1,128,2)/)
   INTEGER(8),  PARAMETER  :: I8(128)=(/(1,0, I=1,128,2)/)
   REAL(4),     PARAMETER  :: R4(128)=(/(1.,0., I=1,128,2)/)
   REAL(8),     PARAMETER  :: R8(128)=(/(1.,0., I=1,128,2)/)
   REAL(16),    PARAMETER  :: R6(128)=(/(1.,0., I=1,128,2)/)
  !LOGICAL(1),  PARAMETER  :: L1(128)=(/(.TRUE., .FALSE., I=1,128,2)/)
  !LOGICAL(2),  PARAMETER  :: L2(128)=(/(.TRUE., .FALSE., I=1,128,2)/)
  !LOGICAL(4),  PARAMETER  :: L4(128)=(/(.TRUE., .FALSE., I=1,128,2)/)
  !LOGICAL(8),  PARAMETER  :: L8(128)=(/(.TRUE., .FALSE., I=1,128,2)/)
  !COMPLEX(4),  PARAMETER  :: Z4(128)=(/((1.,0.),(0.,1.), I=1,128,2)/)
  !COMPLEX(8),  PARAMETER  :: Z8(128)=(/((1.,0.),(0.,1.), I=1,128,2)/)
  !COMPLEX(16), PARAMETER  :: Z6(128)=(/((1.,0.),(0.,1.), I=1,128,2)/)
  !CHARACTER,   PARAMETER  :: C1(128)=(/(ACHAR(1), ACHAR(0), I=1,128,2)/)
  !CHARACTER,   PARAMETER  :: C2(128)=(/(ACHAR(0), ACHAR(1), I=1,128,2)/)

  !INTEGER(1),  PARAMETER  :: II1(128)=(/(0,1, I=1,128,2)/)
   INTEGER(2),  PARAMETER  :: II2(128)=(/(0,1, I=1,128,2)/)
   INTEGER(4),  PARAMETER  :: II4(128)=(/(0,1, I=1,128,2)/)
   INTEGER(8),  PARAMETER  :: II8(128)=(/(0,1, I=1,128,2)/)
   REAL(4),     PARAMETER  :: RR4(128)=(/(0.,1., I=1,128,2)/)
   REAL(8),     PARAMETER  :: RR8(128)=(/(0.,1., I=1,128,2)/)
   REAL(16),    PARAMETER  :: RR6(128)=(/(0.,1., I=1,128,2)/)
  !LOGICAL(1),  PARAMETER  :: LL1(128)=(/(.FALSE., .TRUE., I=1,128,2)/)
  !LOGICAL(2),  PARAMETER  :: LL2(128)=(/(.FALSE., .TRUE., I=1,128,2)/)
  !LOGICAL(4),  PARAMETER  :: LL4(128)=(/(.FALSE., .TRUE., I=1,128,2)/)
  !LOGICAL(8),  PARAMETER  :: LL8(128)=(/(.FALSE., .TRUE., I=1,128,2)/)
  !COMPLEX(4),  PARAMETER  :: ZZ4(128)=(/((0.,1.),(1.,0.), I=1,128,2)/)
  !COMPLEX(8),  PARAMETER  :: ZZ8(128)=(/((0.,1.),(1.,0.), I=1,128,2)/)
  !COMPLEX(16), PARAMETER  :: ZZ6(128)=(/((0.,1.),(1.,0.), I=1,128,2)/)
  !CHARACTER,   PARAMETER  :: CC1(128)=(/(ACHAR(0), ACHAR(1), I=1,128,2)/)
  !CHARACTER,   PARAMETER  :: CC2(128)=(/(ACHAR(1), ACHAR(0), I=1,128,2)/)

  !INTEGER(1),  PARAMETER  :: MI1(128)=(/(-1,0, I=1,128,2)/)
   INTEGER(2),  PARAMETER  :: MI2(128)=(/(-1,0, I=1,128,2)/)
   INTEGER(4),  PARAMETER  :: MI4(128)=(/(-1,0, I=1,128,2)/)
   INTEGER(8),  PARAMETER  :: MI8(128)=(/(-1,0, I=1,128,2)/)
   REAL(4),     PARAMETER  :: MR4(128)=(/(-1.,0., I=1,128,2)/)
   REAL(8),     PARAMETER  :: MR8(128)=(/(-1.,0., I=1,128,2)/)
   REAL(16),    PARAMETER  :: MR6(128)=(/(-1.,0., I=1,128,2)/)
  !LOGICAL(1),  PARAMETER  :: ML1(128)=(/(.TRUE., .FALSE., I=1,128,2)/)
  !LOGICAL(2),  PARAMETER  :: ML2(128)=(/(.TRUE., .FALSE., I=1,128,2)/)
  !LOGICAL(4),  PARAMETER  :: ML4(128)=(/(.TRUE., .FALSE., I=1,128,2)/)
  !LOGICAL(8),  PARAMETER  :: ML8(128)=(/(.TRUE., .FALSE., I=1,128,2)/)
  !COMPLEX(4),  PARAMETER  :: MZ4(128)=(/((1.,0.),(0.,1.), I=1,128,2)/)
  !COMPLEX(8),  PARAMETER  :: MZ8(128)=(/((1.,0.),(0.,1.), I=1,128,2)/)
  !COMPLEX(16), PARAMETER  :: MZ6(128)=(/((1.,0.),(0.,1.), I=1,128,2)/)
  !CHARACTER,   PARAMETER  :: C1(128)=(/(ACHAR(1), ACHAR(0), I=1,128,2)/)
  !CHARACTER,   PARAMETER  :: C2(128)=(/(ACHAR(0), ACHAR(1), I=1,128,2)/)


  END MODULE


  PROGRAM  InitExpCVMGN
  USE M
  IMPLICIT NONE

  INTEGER ::  J

 !INTEGER(KIND())  :: III1(128)
  INTEGER(KIND(CVMGN(TSOURCE=I2, FSOURCE=II2, MASK=MI4)))  :: III2(128) = CVMGN(TSOURCE=I2, FSOURCE=II2, MASK=MI8)
  INTEGER(KIND(CVMGN(TSOURCE=I4, FSOURCE=II4, MASK=MI8)))  :: III4(128) = CVMGN(TSOURCE=I4, FSOURCE=II4, MASK=MI2)
  INTEGER(KIND(CVMGN(TSOURCE=I8, FSOURCE=II8, MASK=MI2)))  :: III8(128) = CVMGN(TSOURCE=I8, FSOURCE=II8, MASK=MI4)
  REAL(KIND(CVMGN(TSOURCE=R4, FSOURCE=RR4, MASK=MR4)))     :: RRR4(128) = CVMGN(TSOURCE=R4, FSOURCE=RR4, MASK=MR4)
  REAL(KIND(CVMGN(TSOURCE=R8, FSOURCE=RR8, MASK=MR8)))     :: RRR8(128) = CVMGN(TSOURCE=R8, FSOURCE=RR8, MASK=MR8)
  REAL(KIND(CVMGN(TSOURCE=R6, FSOURCE=RR6, MASK=MR4)))     :: RRR6(128) = CVMGN(TSOURCE=R6, FSOURCE=RR6, MASK=MR4)
 !LOGICAL(KIND(L1))  :: LLL1(128)
 !LOGICAL(KIND(L2))  :: LLL2(128)
 !LOGICAL(KIND(L4))  :: LLL4(128)
 !LOGICAL(KIND(L8))  :: LLL8(128)
 !COMPLEX(KIND(Z4))  :: ZZZ4(128)
 !COMPLEX(KIND(Z8))  :: ZZZ8(128)
 !COMPLEX(KIND(Z6))  :: ZZZ6(128)
 !CHARACTER(LEN(C1)) :: CCC (128)



! IF ( KIND(III1)   .NE.  1 ) ERROR STOP 11
  IF ( KIND(III2)   .NE.  2 ) ERROR STOP 12
  IF ( KIND(III4)   .NE.  4 ) ERROR STOP 13
  IF ( KIND(III8)   .NE.  8 ) ERROR STOP 14

  IF ( KIND(RRR4)   .NE.  4 ) ERROR STOP 21
  IF ( KIND(RRR8)   .NE.  8 ) ERROR STOP 22
  IF ( KIND(RRR6)   .NE.  16) ERROR STOP 23

! IF ( KIND(LLL1)   .NE.  1 ) ERROR STOP 31
! IF ( KIND(LLL2)   .NE.  2 ) ERROR STOP 32
! IF ( KIND(LLL4)   .NE.  4 ) ERROR STOP 33
! IF ( KIND(LLL8)   .NE.  8 ) ERROR STOP 34

! IF ( KIND(ZZZ4)   .NE.  4 ) ERROR STOP 41
! IF ( KIND(ZZZ8)   .NE.  8 ) ERROR STOP 42
! IF ( KIND(ZZZ6)   .NE.  16) ERROR STOP 43

! IF ( ANY (III1    .NE. 1 )) ERROR STOP 51
  IF ( ANY (III2    .NE. 1 )) ERROR STOP 52
  IF ( ANY (III4    .NE. 1 )) ERROR STOP 53
  IF ( ANY (III8    .NE. 1 )) ERROR STOP 54

  IF ( ANY (RRR4    .NE. 1 )) ERROR STOP 61
  IF ( ANY (RRR8    .NE. 1 )) ERROR STOP 62
  IF ( ANY (RRR6    .NE. 1 )) ERROR STOP 63

! IF ( ANY (LLL1    .NEQV. .TRUE. )) ERROR STOP 71
! IF ( ANY (LLL2    .NEQV. .TRUE. )) ERROR STOP 72
! IF ( ANY (LLL4    .NEQV. .TRUE. )) ERROR STOP 73
! IF ( ANY (LLL8    .NEQV. .TRUE. )) ERROR STOP 74

! IF ( ANY (ZZZ4    .NE. (1.,0.) )) ERROR STOP 81
! IF ( ANY (ZZZ8    .NE. (1.,0.) )) ERROR STOP 82
! IF ( ANY (ZZZ6    .NE. (1.,0.) )) ERROR STOP 83

! IF ( ANY (CCC     .NE. ACHAR(1) )) ERROR STOP 99

  END

