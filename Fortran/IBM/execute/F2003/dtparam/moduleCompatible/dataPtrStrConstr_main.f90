! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp /tstdev/F2003/dataPtrAssgn/CrossFeatures/dataPtrStrConstr.f
! opt variations: -qnok -qnol -qnodeferredlp

!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrStrConstr.f
!*
!*  DATE                       : Feb. 09, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 289075
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Construction of derived-type values
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890
  PROGRAM dataPtrStrConstr
  USE M
  IMPLICIT NONE

  TYPE (DT(4,20)),  TARGET   :: T(10,10)
  TYPE (DT1(4,20)), TARGET   :: T1(10)

  T = DT(4,20)(T%Fun(T))
  SELECT TYPE( As => T(1,1)%Ptr)
  TYPE IS (DT(4,*))
    IF (.NOT. ASSOCIATED(T(1,1)%Ptr, T))                    STOP 11
    IF (ANY( LBOUND(T(1,1)%Ptr)         .NE. (/1, 1 /)))    STOP 12
    IF (ANY( UBOUND(T(1,1)%Ptr)         .NE. (/10,10 /)))    STOP 13
  CLASS DEFAULT
     STOP 14
  END SELECT


  T1 = DT1(4,20)(T1%Fun(T1))
  SELECT TYPE( As => T1(1)%Ptr)
  TYPE IS (DT1(4,*))
    IF (.NOT. ASSOCIATED(T1(1)%Ptr, T1(1:9)))           STOP 21
    IF (ANY( LBOUND(T1(1)%Ptr)         .NE. (/1 /)))    STOP 22
    IF (ANY( UBOUND(T1(1)%Ptr)         .NE. (/9 /)))    STOP 23
  CLASS DEFAULT
     STOP 14
  END SELECT

  END


