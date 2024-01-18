!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2012-06-25
!*
!*  PRIMARY FUNCTIONS TESTED   : FORALL with type specifier (F2008 extension)
!*  SECONDARY FUNCTIONS TESTED :
!*  ADAPTED FROM               : hpf_forall/construct/fxfc024.scenario
!*
!*  DESCRIPTION
!*
!*    Test that the scalar-mask-expr is evaluated only for the valid
!*    combinations of index-names.
!*
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM fxfc024

   implicit none
   INTEGER CASENUM ,i,j,k

   INTEGER SANITY(100)

   INTEGER i1(20,20)
   INTEGER i2(-10:10,-10:10)

   REAL r1(100)
   REAL r2(-50:50)
   REAL r3(20,20)

!--------------------------
! Sanity test
!--------------------------

   CASENUM = 1
   PRINT *, "CASENUM = ",CASENUM

   SANITY = 1
   FORALL ( integer*1::i = 1:100, i .EQ. 1 )
      SANITY(i) = 99
   END FORALL
   PRINT *,SANITY  !- Should all be 99

   SANITY = 1
   FORALL ( integer*2::i = 1:100, i .NE. 1 )
      SANITY(i) = 99
   END FORALL
   PRINT *,SANITY  !- Should all be 1

!--------------------------
! Full evaluation of mask is possible
!--------------------------

   CASENUM = 2
   PRINT *, "CASENUM = ",CASENUM

   i1 = 0
   FORALL ( integer*1::i = 1:20, j = 1:20, i > j )
      i1(i,j) = i*20+j
   END FORALL
   PRINT *,i1

   i2 = 0
   FORALL ( i = -10:10, j = -10:10 )
      i2(i,j) = i*20 + j
   END FORALL

   FORALL ( integer*2::i = -10:10, j = -10:10, i2(i,j) .NE. 0 )
      i2(i,j) = 99
   END FORALL
   PRINT *,i2

!--------------------------
! Full evaluation of mask results in error
!--------------------------

   CASENUM = 3
   PRINT *, "CASENUM = ",CASENUM

   r1 = 1.0
   forall ( integer*2::i = 1:100:2 ) r1(i) = i

   FORALL ( integer*4::i = 1:100:2, REAL(i)/r1(i) .GT. 5 )   ! -  Full evaluation results in
      r1(i) = -99.0                            !    a divide by zero
   END FORALL
   PRINT *,r1

   r2 = 1.0
   forall ( integer*1::i = -40:40 ) r2(i) = i
   PRINT *,r2

   FORALL ( integer*2::i = -40:40 , REAL(i)/r2(i) .LT. 1 )
      r2(i) = -99.0
   END FORALL
   PRINT *,r2


   CASENUM = 4
   PRINT *, "CASENUM = ",CASENUM

   r3 = 1.0
   forall ( integer*8::i = 1:20:2, j = 2:20:2 ) r3(i,j) = 1.0
   PRINT *,r3 !- Verify correctness

   FORALL ( integer*2::i = 2:20:2, j = 1:20:2 , 1.0 / r3(j,i) .EQ. 1.0 )
      r3(i,j) = 99
   END FORALL
   PRINT *,r3

END PROGRAM



