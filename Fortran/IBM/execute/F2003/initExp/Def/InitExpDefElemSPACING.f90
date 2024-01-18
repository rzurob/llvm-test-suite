!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefElemSPACING.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Apr. 14, 2006
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
!*  a reference to an elemental intrinsic
!* 
!*  -  SPACING 
!*  (319281)
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM  InitExpDefElemSPACING
  IMPLICIT NONE 
  INTEGER :: I, J


  REAL(4), PARAMETER :: r4Quiet_N_NaN     = REAL(z"FFFFFFFF")
  REAL(4), PARAMETER :: r4Signaling_N_NaN = REAL(z"FFBFFFFF")
  REAL(4), PARAMETER :: r4N_Infinity      = REAL(z"FF800000")
  REAL(4), PARAMETER :: r4N_DeNormalized  = REAL(z"FF7FFFFF")
  REAL(4), PARAMETER :: r4N_Zero          = REAL(z"80000000")
  REAL(4), PARAMETER :: r4P_Zero          = REAL(z"00000000")
  REAL(4), PARAMETER :: r4P_DeNormalized  = REAL(z"00000001")
  REAL(4), PARAMETER :: r4P_Infinity      = REAL(z"7F800000")
  REAL(4), PARAMETER :: r4Signaling_P_NaN = REAL(z"7F800001")
  REAL(4), PARAMETER :: r4Quiet_P_NaN     = REAL(z"7FC00000")

  REAL(8), PARAMETER :: r8Quiet_N_NaN     = REAL(z"FFFFFFFFFFFFFFFF", KIND=8)
  REAL(8), PARAMETER :: r8Signaling_N_NaN = REAL(z"FFF7FFFFFFFFFFFF", KIND=8)
  REAL(8), PARAMETER :: r8N_Infinity      = REAL(z"FFF0000000000000", KIND=8)
  REAL(8), PARAMETER :: r8N_DeNormalized  = REAL(z"800FFFFFFFFFFFFF", KIND=8)
  REAL(4), PARAMETER :: r8N_Zero          = REAL(z"8000000000000000", KIND=8)
  REAL(4), PARAMETER :: r8P_Zero          = REAL(z"0000000000000000", KIND=8)
  REAL(8), PARAMETER :: r8P_DeNormalized  = REAL(z"0000000000000001", KIND=8)
  REAL(8), PARAMETER :: r8P_Infinity      = REAL(z"7FF0000000000000", KIND=8)
  REAL(8), PARAMETER :: r8Signaling_P_NaN = REAL(z"7FF0000000000001", KIND=8)
  REAL(8), PARAMETER :: r8Quiet_P_NaN     = REAL(z"7FF8000000000000", KIND=8)

  INTEGER(4), PARAMETER :: i4Quiet_N_NaN     = INT(z"FFFFFFFF")
  INTEGER(4), PARAMETER :: i4Signaling_N_NaN = INT(z"FFBFFFFF")
  INTEGER(4), PARAMETER :: i4N_Infinity      = INT(z"FF800000")
  INTEGER(4), PARAMETER :: i4N_DeNormalized  = INT(z"FF7FFFFF")
  INTEGER(4), PARAMETER :: i4N_Zero          = INT(z"80000000")
  INTEGER(4), PARAMETER :: i4P_Zero          = INT(z"00000000")
  INTEGER(4), PARAMETER :: i4P_DeNormalized  = INT(z"00000001")
  INTEGER(4), PARAMETER :: i4P_Infinity      = INT(z"7F800000")
  INTEGER(4), PARAMETER :: i4Signaling_P_NaN = INT(z"7F800001")
  INTEGER(4), PARAMETER :: i4Quiet_P_NaN     = INT(z"7FC00000")

  INTEGER(8), PARAMETER :: i8Quiet_N_NaN     = INT(z"FFFFFFFFFFFFFFFF", KIND=8)
  INTEGER(8), PARAMETER :: i8Signaling_N_NaN = INT(z"FFF7FFFFFFFFFFFF", KIND=8)
  INTEGER(8), PARAMETER :: i8N_Infinity      = INT(z"FFF0000000000000", KIND=8)
  INTEGER(8), PARAMETER :: i8N_DeNormalized  = INT(z"800FFFFFFFFFFFFF", KIND=8)
  INTEGER(4), PARAMETER :: i8N_Zero          = INT(z"8000000000000000", KIND=8)
  INTEGER(4), PARAMETER :: i8P_Zero          = INT(z"0000000000000000", KIND=8)
  INTEGER(8), PARAMETER :: i8P_DeNormalized  = INT(z"0000000000000001", KIND=8)
  INTEGER(8), PARAMETER :: i8P_Infinity      = INT(z"7FF0000000000000", KIND=8)
  INTEGER(8), PARAMETER :: i8Signaling_P_NaN = INT(z"7FF0000000000001", KIND=8)
  INTEGER(8), PARAMETER :: i8Quiet_P_NaN     = INT(z"7FF8000000000000", KIND=8)


  TYPE :: DT
    REAL(4)  :: R4(4,4)=RESHAPE((/(3.0, I=1,16)/),(/4,4/)) 
    REAL(8)  :: R8(4,4)=RESHAPE((/(3.0, I=1,16)/),(/4,4/)) 
    REAL(16) :: R6(4,4)=RESHAPE((/(3.0, I=1,16)/),(/4,4/)) 
  END TYPE

  TYPE (DT), PARAMETER :: X = DT()
  REAL(4),   PARAMETER :: RR4=2._4**(-22)
  REAL(8),   PARAMETER :: RR8=2._8**(-51)
  REAL(16),  PARAMETER :: RR6=2._16**(-104)

  REAL(KIND(RESHAPE(X%R4,(/4,4/))))  :: TR4(4,4)=RESHAPE(SPACING(X=X%R4),(/4,4/)) 
  REAL(KIND(RESHAPE(X%R8,(/4,4/))))  :: TR8(4,4)=RESHAPE(SPACING(X=X%R8),(/4,4/)) 
  REAL(KIND(RESHAPE(X%R6,(/4,4/))))  :: TR6(4,4)=RESHAPE(SPACING(X=X%R6),(/4,4/)) 

  REAL(4) :: T41(8)=SPACING(X=r4Quiet_N_NaN)
  REAL(4) :: T42(8)=SPACING(X=r4Signaling_N_NaN)
  REAL(4) :: T43(8)=SPACING(X=r4N_Infinity)
  REAL(4) :: T44(8)=SPACING(X=r4N_Zero)
  REAL(4) :: T45(8)=SPACING(X=r4P_Zero)
  REAL(4) :: T46(8)=SPACING(X=r4P_Infinity)
  REAL(4) :: T47(8)=SPACING(X=r4Signaling_P_NaN)
  REAL(4) :: T48(8)=SPACING(X=r4Quiet_P_NaN)

  REAL(8) :: T81(8)=SPACING(X=r8Quiet_N_NaN)
  REAL(8) :: T82(8)=SPACING(X=r8Signaling_N_NaN)
  REAL(8) :: T83(8)=SPACING(X=r8N_Infinity)
  REAL(8) :: T84(8)=SPACING(X=r8N_Zero)
  REAL(8) :: T85(8)=SPACING(X=r8P_Zero)
  REAL(8) :: T86(8)=SPACING(X=r8P_Infinity)
  REAL(8) :: T87(8)=SPACING(X=r8Signaling_P_NaN)
  REAL(8) :: T88(8)=SPACING(X=r8Quiet_P_NaN)



  IF ( KIND(TR4)  .NE.    4 )  STOP 11
  IF ( KIND(TR8)  .NE.    8 )  STOP 12
  IF ( KIND(TR6)  .NE.   16 )  STOP 13

  IF (ANY( TR4  .NE. RR4  )) STOP 21
  IF (ANY( TR8  .NE. RR8  )) STOP 22
  IF (ANY( TR6  .NE. RR6  )) STOP 23


  IF ( ANY( TRANSFER(T41, i4Quiet_N_NaN, 8)      .NE. i4Quiet_N_NaN      ) ) STOP 11
  IF ( ANY( TRANSFER(T42, i4Signaling_N_NaN, 8)  .NE. i4Signaling_N_NaN  ) ) STOP 12
  IF ( ANY( TRANSFER(T43, i4N_Infinity, 8)       .NE. i4P_Infinity       ) ) STOP 13
  IF ( ANY( T44                                  .NE. TINY(r4N_Zero)     ) ) STOP 14
  IF ( ANY( T45                                  .NE. TINY(r4N_Zero)     ) ) STOP 15
  IF ( ANY( TRANSFER(T46, i4P_Infinity, 8)       .NE. i4P_Infinity       ) ) STOP 16
  IF ( ANY( TRANSFER(T47, i4Signaling_P_NaN, 8)  .NE. i4Signaling_P_NaN  ) ) STOP 17
  IF ( ANY( TRANSFER(T48, i4Quiet_P_NaN, 8)      .NE. i4Quiet_P_NaN      ) ) STOP 18


  IF ( ANY( TRANSFER(T81, i8Quiet_N_NaN, 8)      .NE. i8Quiet_N_NaN      ) ) STOP 21
  IF ( ANY( TRANSFER(T82, i8Signaling_N_NaN, 8)  .NE. i8Signaling_N_NaN  ) ) STOP 22
  IF ( ANY( TRANSFER(T83, i8N_Infinity, 8)       .NE. i8P_Infinity       ) ) STOP 23
  IF ( ANY( T84                                  .NE. TINY(r8N_Zero)     ) ) STOP 24
  IF ( ANY( T85                                  .NE. TINY(r8N_Zero)     ) ) STOP 25
  IF ( ANY( TRANSFER(T86, i8P_Infinity, 8)       .NE. i8P_Infinity       ) ) STOP 26
  IF ( ANY( TRANSFER(T87, i8Signaling_P_NaN, 8)  .NE. i8Signaling_P_NaN  ) ) STOP 27
  IF ( ANY( TRANSFER(T88, i8Quiet_P_NaN, 8)      .NE. i8Quiet_P_NaN      ) ) STOP 28




  END


