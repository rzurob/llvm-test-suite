!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefElemALL.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Mar 25, 2006
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
!*  a reference to a transformational intrinsic
!* 
!*  - ALL 
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  InitExpDefElemALL 
  IMPLICIT NONE
  INTEGER :: I, J, K

  INTEGER(1),   PARAMETER :: B1(2,3)=RESHAPE((/1,2,3,4,5,6/),(/2,3/))
  INTEGER(2),   PARAMETER :: B2(2,3)=B1
  INTEGER(4),   PARAMETER :: B4(2,3)=B2
  INTEGER(8),   PARAMETER :: B8(2,3)=B4

  INTEGER(1),   PARAMETER :: C1(2,3)=RESHAPE((/0,7,3,4,5,8/),(/2,3/))
  INTEGER(2),   PARAMETER :: C2(2,3)=C1
  INTEGER(4),   PARAMETER :: C4(2,3)=C2
  INTEGER(8),   PARAMETER :: C8(2,3)=C4

  LOGICAL,      PARAMETER :: R1(3) =RESHAPE((/.TRUE.,  .FALSE., .FALSE./),(/3/))
  LOGICAL,      PARAMETER :: R3(2) =RESHAPE((/.FALSE., .FALSE./),(/2/))
  LOGICAL(1),   PARAMETER :: R21(2)=RESHAPE((/.TRUE.,  .TRUE./),(/2/))
  LOGICAL(2),   PARAMETER :: R22(2)=R21
  LOGICAL(4),   PARAMETER :: R24(2)=R22
  LOGICAL(8),   PARAMETER :: R28(2)=R24

  LOGICAL(KIND(ALL(R21))), PARAMETER :: T11(2)=ALL(R21)
  LOGICAL(KIND(ALL(R22))), PARAMETER :: T12(2)=ALL(R22)
  LOGICAL(KIND(ALL(R24))), PARAMETER :: T14(2)=ALL(R24 .EQV. R24)
  LOGICAL(KIND(ALL(R28))), PARAMETER :: T18(2)=ALL(R28 .EQV. R21)

  LOGICAL(1), PARAMETER   :: Z1(1:0, 2)= .FALSE.
  LOGICAL(2), PARAMETER   :: Z2(1:0, 2)= .FALSE.
  LOGICAL(4), PARAMETER   :: Z4(1:0, 2)= .FALSE.
  LOGICAL(8), PARAMETER   :: Z8(1:0, 2)= .FALSE.
 
  LOGICAL(KIND(ALL(T11))), PARAMETER :: T21(1:0)=ALL(Z1)
  LOGICAL(KIND(ALL(T12))), PARAMETER :: T22(1:0)=ALL(Z2)
  LOGICAL(KIND(ALL(T14))), PARAMETER :: T24(1:0)=ALL(Z4)
  LOGICAL(KIND(ALL(T18))), PARAMETER :: T28(1:0)=ALL(Z8)

  LOGICAL(KIND(ALL(T21))), PARAMETER :: T31(3)=ALL((B1 .NE. C1), 1)
  LOGICAL(KIND(ALL(T22))), PARAMETER :: T32(3)=ALL((B2 .NE. C2), 1)
  LOGICAL(KIND(ALL(T24))), PARAMETER :: T34(3)=ALL((B4 .NE. C4), 1)
  LOGICAL(KIND(ALL(T28))), PARAMETER :: T38(3)=ALL((B8 .NE. C8), 1)

  LOGICAL(KIND(ALL(T31))) :: T41(2)=ALL((B1 .NE. C1), 2)
  LOGICAL(KIND(ALL(T32))) :: T42(2)=ALL((B2 .NE. C2), 2)
  LOGICAL(KIND(ALL(T34))) :: T44(2)=ALL((B4 .NE. C4), 2)
  LOGICAL(KIND(ALL(T38))) :: T48(2)=ALL((B8 .NE. C8), 2)


  IF (KIND(T11)   .NE.   1 )        STOP 11
  IF (ANY(T11     .NEQV. .TRUE.))   STOP 12
  IF (KIND(T12)   .NE.   2 )        STOP 13
  IF (ANY(T12     .NEQV. .TRUE.))   STOP 14
  IF (KIND(T14)   .NE.   4 )        STOP 15
  IF (ANY(T14     .NEQV. .TRUE.))   STOP 16
  IF (KIND(T18)   .NE.   8 )        STOP 17
  IF (ANY(T18     .NEQV. .TRUE.))   STOP 18


  IF (KIND(T21)   .NE.   1 )        STOP 21
  IF (ANY (T21    .NEQV. .TRUE.))   STOP 12
  IF (KIND(T22)   .NE.   2 )        STOP 23
  IF (ANY (T22    .NEQV. .TRUE.))   STOP 14
  IF (KIND(T24)   .NE.   4 )        STOP 25
  IF (ANY (T24    .NEQV. .TRUE.))   STOP 16
  IF (KIND(T28)   .NE.   8 )        STOP 27
  IF (ANY (T28    .NEQV. .TRUE.))   STOP 18

  IF ( ANY( T31 .NEQV. R1) )   STOP 31
  IF ( ANY( T32 .NEQV. R1) )   STOP 32
  IF ( ANY( T34 .NEQV. R1) )   STOP 33
  IF ( ANY( T38 .NEQV. R1) )   STOP 34

  IF ( ANY( T41 .NEQV. R3) )   STOP 41
  IF ( ANY( T42 .NEQV. R3) )   STOP 42
  IF ( ANY( T44 .NEQV. R3) )   STOP 43
  IF ( ANY( T48 .NEQV. R3) )   STOP 44



  END


 
