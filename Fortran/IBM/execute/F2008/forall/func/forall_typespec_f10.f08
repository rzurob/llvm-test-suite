!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2012-06-25
!*
!*  PRIMARY FUNCTIONS TESTED   : FORALL with type specifier (F2008 extension)
!*  SECONDARY FUNCTIONS TESTED :
!*  ADAPTED FROM               : hpf_forall/construct/fxfc026.scenario
!*
!*  DESCRIPTION
!*
!*    Test that execution of the forall-body statements occur only for active
!*    combinations of index-name values.
!*
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM fxfc026


   INTEGER CASENUM ,i,j,k

   REAL TEST1(100)
   REAL TEST2(20,20)

!----------------------------------
! Simple forall no stride no mask
!----------------------------------

   CASENUM = 1
   PRINT *,"CASENUM = ",CASENUM

   TEST1 = 0
   FORALL ( integer(1)::i = 1:100 )
      TEST1(i) = 1
   END FORALL
   PRINT *,TEST1

!----------------------------------
! Simple forall with stride no mask
!----------------------------------

   CASENUM = 2
   PRINT *,"CASENUM = ",CASENUM

   TEST1 = 0
   FORALL ( integer(2)::i = 1:100:2 )
      TEST1(i) = 1
   END FORALL

   TEST2 = 0
   FORALL ( integer(4)::i = 1:20 )
      FORALL ( integer(2)::j = i:20 )
         TEST2(i,j) = 1
      END FORALL
   END FORALL

   PRINT *,TEST1
   PRINT *,TEST2

!----------------------------------
! Simple forall no stride with mask
!----------------------------------

   CASENUM = 3
   PRINT *,"CASENUM = ",CASENUM

   TEST1 = 0
   FORALL ( integer(1)::i = 1:100, i > 50 )
      TEST1(i) = 1
   END FORALL
   PRINT *,TEST1

   TEST2 = 0
   FORALL ( integer(2):: i = 1:20, j = 1:20, i < j )
      TEST2(i,j) = 1
   END FORALL
   PRINT *,TEST2

   TEST2 = 0
   j = 17
   FORALL ( integer(4)::i = 1:20, i < j )
      FORALL ( integer(2)::j = 1:20, i < j )
         TEST2(i,j) = 1
      END FORALL
   END FORALL
   PRINT *, TEST2


!----------------------------------
! forall with stride with mask
!----------------------------------

   CASENUM = 4
   PRINT *,"CASENUM = ",CASENUM

   TEST1 = 0
   FORALL ( integer(1)::i = 1:100:5, i > 40 )
      TEST1(i) = 1
   END FORALL
   PRINT *,TEST1

   TEST2 = 0

   FORALL ( integer(2)::i = 1:20:2, j = 20:1:-2, i > j )
      TEST2(i,j) = 1

   END FORALL
   PRINT *,TEST2

END PROGRAM

