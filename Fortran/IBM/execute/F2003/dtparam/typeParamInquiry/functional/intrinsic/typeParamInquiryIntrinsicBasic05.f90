!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : typeParamInquiryIntrinsicBasic05.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : July 6 2008 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : TYPE PARAMETER INQUIRY
!*
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  REFERENCE                  : 
!*
!*  DRIVER STANZA              : xlf2003
!*
!*
!*  DESCRIPTION
!*
!* 
!* 1. TEST SECTION 6.1.3
!* 2. TYPE PARAMETER INQUIRY FOR INTRINSIC TYPE 
!* 3. TYPE PARAMETER ARE CONSTANT OR EXPRESSION
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  PROGRAM typeParamInquiryIntrinsicBasic05
    IMPLICIT NONE
    
    INTEGER,PARAMETER :: K1=1,K2=2,K4=4,K8=8,K16=16
    INTEGER(K1%KIND+KIND(K1))  :: I1
    INTEGER(2_4)               :: I2
    INTEGER(KIND(4_K8))        :: I3 
    INTEGER(2*4)               :: I4

    REAL(4_8)         :: R1
    REAL(8_K8)        :: R2
    REAL(max(8,K8))   :: R3
 
    COMPLEX(K4%KIND)       :: X1
    COMPLEX(KIND(8_K4)+4)  :: X2
    COMPLEX(K8+K8)         :: X3

    LOGICAL(k2%kind+k1%kind)    :: L1
    LOGICAL(2_1)      :: L2
    LOGICAL(K2**2)    :: L3
    LOGICAL(8)        :: L4

    CHARACTER(2)      :: C1
    CHARACTER(K4,1_4) :: C2
    CHARACTER(-10)    :: C3   !length is negative here 
    CHARACTER(K8*2_1,1**1)    :: C4
    CHARACTER(C2%LEN,KIND(C2)) :: C5
    CHARACTER(LEN(C4),KIND(C4)) :: C6

    IF( I1%KIND /= KIND(I1) .or. I1%KIND /= 8 .or. KIND(I1) /= 8 ) STOP 1
    IF( I2%KIND /= KIND(I2) .or. I2%KIND /= 2 .or. KIND(I2) /= 2 ) STOP 2 
    IF( I3%KIND /= KIND(I3) .or. I3%KIND /= 8 .or. KIND(I3) /= 8 ) STOP 3 
    IF( I4%KIND /= KIND(I4) .or. I4%KIND /= 8 .or. KIND(I4) /= 8 ) STOP 4 

    IF( R1%KIND /= KIND(R1) .or. R1%KIND /= 4 .or. KIND(R1) /= 4 ) STOP 5 
    IF( R2%KIND /= KIND(R2) .or. R2%KIND /= 8 .or. KIND(R2) /= 8 ) STOP 6 
    IF( R3%KIND /= KIND(R3) .or. R3%KIND /= 8 .or. KIND(R3) /= 8)  STOP 7 

    IF( X1%KIND /= KIND(X1) .or. X1%KIND /= 4 .or. KIND(X1) /= 4 ) STOP 8 
    IF( X2%KIND /= KIND(X2) .or. X2%KIND /= 8 .or. KIND(X2) /= 8 ) STOP 9 
    IF( X3%KIND /= KIND(X3) .or. X3%KIND /=16 .or. KIND(X3) /= 16) STOP 10 

    IF( L1%KIND /= KIND(L1) .or. L1%KIND /= 8 .or. KIND(L1) /= 8 ) STOP 11 
    IF( L2%KIND /= KIND(L2) .or. L2%KIND /= 2 .or. KIND(L2) /= 2 ) STOP 12 
    IF( L3%KIND /= KIND(L3) .or. L3%KIND /= 4 .or. KIND(L3) /= 4 ) STOP 13 
    IF( L4%KIND /= KIND(L4) .or. L4%KIND /= 8 .or. KIND(L4) /= 8 ) STOP 14 

    IF( C1%KIND /= KIND(C1) .or. C1%KIND /= 1 .or. KIND(C1) /= 1 ) STOP 15 
    IF( C1%LEN  /= LEN(C1)  .or. C1%LEN  /= 2 .or. LEN(C1)  /= 2 ) STOP 16 

    IF( C2%KIND /= KIND(C2) .or. C2%KIND /= 1 .or. KIND(C2) /= 1 ) STOP 17 
    IF( C2%LEN  /= LEN(C2)  .or. C2%LEN  /= 4 .or. LEN(C2)  /= 4 ) STOP 18
  
    IF( C3%KIND /= KIND(C3) .or. C3%KIND /= 1 .or. KIND(C3) /= 1 ) STOP 19
    IF( C3%LEN  /= LEN(C3)  .or. C3%LEN  /= 0 .or. LEN(C3)  /= 0 ) STOP 20    

    IF( C4%KIND /= KIND(C4) .or. C4%KIND /= 1 .or. KIND(C4) /= 1 ) STOP 21 
    IF( C4%LEN  /= LEN(C4)  .or. C4%LEN  /=16 .or. LEN(C4)  /=16 ) STOP 22 

    IF( C5%KIND /= KIND(C5) .or. C5%KIND /= 1 .or. KIND(C5) /= 1 ) STOP 23
    IF( C5%LEN  /= LEN(C5)  .or. C5%LEN  /= 4 .or. LEN(C5)  /= 4 ) STOP 24   

    IF( C6%KIND /= KIND(C6) .or. C6%KIND /= 1 .or. KIND(C6) /= 1 ) STOP 25
    IF( C6%LEN  /= LEN(C6)  .or. C6%LEN  /=16 .or. LEN(C6) /= 16 ) STOP 26


  END

