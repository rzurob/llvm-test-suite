!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : forall_typespec_f14.f
!*
!*  DATE                       : 2012-06-25
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   : FORALL with type specifier (F2008 extension)
!*  SECONDARY FUNCTIONS TESTED :
!*  ADAPTED FROM               : hpf_forall/statement/fxfs0003b.scenario
!*
!*  DESCRIPTION
!*
!*    Test forall-header with various primarys in scalar-mask-expr that are
!*    dependent on index-name values.  Use simple forall-assignment.
!*
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

      PROGRAM fxfs0003b

      integer ind1,ind2,ind3
      integer i,j,k,CASENUM


      integer ia(100),ib(100,100),ic(-40:60,-50:50)

      logical la(100)

      real    ra(-20:19,-20:19)

      real*8  r8a(20,40),r8b(40,20)

      complex ca(100)

      !----------------------------
      !-  Simple masks, single index
      !----------------------------

      CASENUM = 1
      PRINT *,"CASENUM = ",CASENUM
      ia=0
      forall(integer(1)::ind1=1:100,ind1 .GT. 0) ia(ind1) = ind1
      DO ind1 = 1,100
         PRINT *,ia(ind1)
      END DO

      CASENUM = 2
      PRINT *,"CASENUM = ",CASENUM
      ia=0
      forall(integer(2)::ind1=1:100,ia(ind1) .EQ. 0) ia(ind1) = ind1
      DO ind1 = 1,100
         PRINT *,ia(ind1)
      END DO

      CASENUM = 3
      PRINT *,"CASENUM = ",CASENUM
      ia=0
      do i=1,100,2
         ia(i) = 1
      end do
      forall(integer(4)::ind1=1:100,ia(ind1) .NE. 0) ia(ind1) = ind1
      DO ind1 = 1,100
         PRINT *,ia(ind1)
      END DO

      CASENUM = 4
      PRINT *,"CASENUM = ",CASENUM
      ia=0
      do i=1,100,2
         ia(i) = 1
      end do
      forall(integer::ind1=2:100:2,ia(ind1) .NE. 0) ia(ind1) = ind1
      DO ind1 = 1,100
         PRINT *,ia(ind1)
      END DO

      CASENUM = 5
      PRINT *,"CASENUM = ",CASENUM
      ia=0
      la=.FALSE.
      do i=1,100,2
         la(i) = .TRUE.
      end do
      forall(integer(4)::ind1=1:100,la(ind1)) ia(ind1) = ind1
      DO ind1 = 1,100
         PRINT *,ia(ind1)
      END DO

      CASENUM = 6
      PRINT *,"CASENUM = ",CASENUM
      ca = (0.0,0.0)
      forall(integer(2)::ind1=10:100:10,ind1.NE.100)
     +   ca(ind1) = ( real(ind1),real(ind1) )
      DO ind1 = 1,100
         PRINT *,ca(ind1)
      END DO

      CASENUM = 7
      PRINT *,"CASENUM = ",CASENUM
      ca = (0.0,0.0)
      forall(integer(1)::ind1=10:100:10,ind1.NE.100)
     +   ca(ind1-9:ind1) = ( real(ind1),real(ind1) )
      DO ind1 = 1,100
         PRINT *,ca(ind1)
      END DO

      CASENUM = 8
      PRINT *,"CASENUM = ",CASENUM
      ca = (0.0,0.0)
      forall(integer(2)::ind1=10:100:10,ind1.EQ.50)
     +   ca = ( real(ind1),real(ind1) )
      DO ind1 = 1,100
         PRINT *,ca(ind1)
      END DO

      !----------------------------
      !-  Simple masks, double index
      !----------------------------

      CASENUM = 9
      PRINT *,"CASENUM = ",CASENUM
      ra=0.0
      forall(integer(4)::ind1=-20:19,ind2=-20:19,ind2>ind1)
     +   ra(ind1,ind2) = real( ind2*40+ind1 )
      DO ind1 = -20,19
         DO ind2 = -20,19
            PRINT *,ra(ind1,ind2)
         END DO
      END DO

      CASENUM = 10
      PRINT *,"CASENUM = ",CASENUM
      r8a=0.0
      r8b=0.0
      forall(integer::ind1=1:20,ind2=1:40)
     + r8a(ind1,ind2) = REAL (ind2*20+ind1)
      forall(integer(4)::ind1=1:20,ubound(r8b,2)/5.NE.ind1)
     +   r8b(:,ind1) = r8a(ind1,:)
      DO ind1 = 1,40
         DO ind2 = 1,20
            PRINT *,r8b(ind1,ind2)
         END DO
      END DO

      END
