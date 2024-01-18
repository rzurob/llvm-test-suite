!*  ===================================================================
!*
!*                               Non-CHARACTER Intrinsic Types
!*
!*  DATE                       : August  3, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Type Array Definition
!*  SECONDARY FUNCTIONS TESTED : with Array Constructor Initialization
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : COMPLEX, Intrinsic CMPLX(), and Intrinsic
!*                               RESHAPE()
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*  Currently emits the following Diagnostic Messages:
!*
!*  "unAllocatedAllocatable04d.f", 1517-006 (U) Parse stack overflow.  Expression is too long.  Reduce program size or nested references.
!*  1501-511  Compilation failed for file unAllocatedAllocatable04d.f.
!*
!*  NOTE:  This may be another instance of Defect 317796 - F2003: Derived
!*         Type Array Constructor Expression Too Long
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM unAllocatedAllocatable04d

    COMPLEX(4) :: complexArray( 100,100 ) = RESHAPE(&
        (/ (CMPLX((i * 3.14), (-i / 3.14), 4), i = -4999, 5000) /),&
                                                        (/ 100, 100 /))

END PROGRAM unAllocatedAllocatable04d
