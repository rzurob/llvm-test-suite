!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : typeParamInquiryIntrinsicBasic04.f   
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
!*  DRIVER STANZA              :xlf2003
!*
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 6.1.3 
!* 2. TYPE PARAMETER INQUIRY FOR INTRINSIC TYPE 
!* 3. TYPE PARAMETER IS NOT SPECIFIED  
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  PROGRAM typeParamInquiryIntrinsicBasic04
  IMPLICIT NONE

    INTEGER :: I1,I2
    REAL    :: R1,R2
    DOUBLE PRECISION  :: D1,D2 
    COMPLEX     :: X1,X2
    LOGICAL     :: L1,L2
    CHARACTER   :: C1,C2
 
    IF( I1%KIND /= KIND(I1) .or. I1%KIND /= 4 .or. KIND(I1) /= 4 ) STOP 1
    IF( R1%KIND /= KIND(R1) .or. R1%KIND /= 4 .or. KIND(R1) /= 4 ) STOP 2
    IF( D1%KIND /= KIND(D1) .or. D1%KIND /= 8 .or. KIND(D1) /= 8 ) STOP 3
    IF( L1%KIND /= KIND(L1) .or. L1%KIND /= 4 .or. KIND(L1) /= 4 ) STOP 4
    IF( C1%KIND /= KIND(C1) .or. C1%KIND /= 1 .or. KIND(C1) /= 1 ) STOP 5
    IF( C1%LEN  /= LEN(C1)  .or. C1%LEN  /= 1 .or. LEN(C1)  /= 1 ) STOP 6
    IF( X1%KIND /= KIND(X1) .or. X1%KIND /= 4 .or. KIND(X1) /= 4 ) STOP 7 
   
    CALL SUB1() 

    IF( I2%KIND /= KIND(I2) .or. I2%KIND /= 4 .or. KIND(I2) /= 4 ) STOP 11 
    IF( R2%KIND /= KIND(R2) .or. R2%KIND /= 4 .or. KIND(R2) /= 4 ) STOP 12 
    IF( D2%KIND /= KIND(D2) .or. D2%KIND /= 8 .or. KIND(D2) /= 8 ) STOP 13 
    IF( L2%KIND /= KIND(L2) .or. L2%KIND /= 4 .or. KIND(L2) /= 4 ) STOP 14 
    IF( C2%KIND /= KIND(C2) .or. C2%KIND /= 1 .or. KIND(C2) /= 1 ) STOP 15 
    IF( C2%LEN  /= LEN(C2)  .or. C2%LEN  /= 1 .or. LEN(C2)  /= 1 ) STOP 16 
    IF( X2%KIND /= KIND(X2) .or. X2%KIND /= 4 .or. KIND(X2) /= 4 ) STOP 17
 
    CONTAINS

    SUBROUTINE SUB1()

      I2 = REAL(I2)
      R2 = INT(R2)
      D2 = CMPLX(D2)
      X2 = DBLE(X2)
      L2 = LOGICAL(L2)
      C2 = CHAR(ICHAR(C2)) 
 
    END SUBROUTINE  
   
  END

