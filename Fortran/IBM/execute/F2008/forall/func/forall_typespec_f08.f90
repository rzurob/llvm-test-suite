!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : forall_typespec_f08.f
!*
!*  DATE                       : 2012-06-25
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   : FORALL with type specifier (F2008 extension)
!*  SECONDARY FUNCTIONS TESTED : Multiple index variables in header
!*  ADAPTED FROM               : hpf_forall/construct/fxfc027.scenario
!*
!*  DESCRIPTION
!*
!*    Test that statements in the forall assignment evaluate the expression for
!*    all active combinations and then these values are assigned to the
!*    corresponding locations.
!*
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM fxfc027

   INTEGER CASENUM,i,j,k

   INTEGER SANITY(100)
   INTEGER i1(100)
   INTEGER i2(20,20)

   INTEGER i3(100)
   INTEGER i4(100)
   EQUIVALENCE (i3,i4)

   INTEGER i5(10,10)
   INTEGER i6(100)
   EQUIVALENCE (i5,i6)


   INTEGER i7(100)
   INTEGER i8(100)
   INTEGER i9(200)

   COMMON /BLOCK1/ i7,i8
   EQUIVALENCE (i7,i9)

!-------------------------------------
! Sanity case
!-------------------------------------

   CASENUM = 1
   PRINT *,"CASENUM = ",CASENUM

   FORALL ( integer(2)::i = 1:100 )
      SANITY(i) = i
   END FORALL
   PRINT *,SANITY

!-------------------------------------
! Test destructive overlap for continuity
!-------------------------------------

   CASENUM = 2
   PRINT *,"CASENUM = ",CASENUM

   DO i = 1,100
      i1(i) = i
   END DO
   FORALL ( integer::i = 2:100 )
      i1(i) = i1(i-1)    ! Should have different effect then a do loop
   END FORALL
   PRINT *,i1

   CASENUM = 3
   PRINT *,"CASENUM = ",CASENUM

   FORALL ( integer(2)::i = 1:100 )
      i1(i) = i
      i1(101-i) = i1(i)
   END FORALL
   PRINT *,i1

   DO i = 1,20
      DO j = 1,20
        i2(i,j) = i*20 + j
      END DO
   END DO

  DO i = 1,20
     DO j = 1,20
        i2(i,j) = i*20 + j
     END DO
  END DO

   CASENUM = 4
   PRINT *,"CASENUM = ",CASENUM

  FORALL ( integer(2)::i = 1:20, j = 1:20 )
     i2(j,i) = i2(i,j)
  END FORALL
  PRINT *,i2

!-------------------------------------
! Test destructive array overlap
!-------------------------------------

   CASENUM = 5
   PRINT *,"CASENUM = ",CASENUM

   DO i = 1,20
      DO j = 1,20
         i2(i,j) = i*20 + j
      END DO
   END DO

   FORALL ( integer(2)::i = 1:20 )
      i2(21-i,:) = i2(i,:)
   END FORALL
   PRINT *,i2

   CASENUM = 6
   PRINT *,"CASENUM = ",CASENUM

   DO i = 1,20
      DO j = 1,20
         i2(i,j) = i*20 + j
      END DO
   END DO

   FORALL ( integer(1)::i = 1:20, j = 1:20 )
      i2(i,j) = SUM( i2 )
   END FORALL
   PRINT *,i2

!-------------------------------------
! Test destructive array overlap
! via equivalence
!-------------------------------------

   CASENUM = 7
   PRINT *,"CASENUM = ",CASENUM

   DO i = 1,100
      i3(i) = i
   END DO

   FORALL ( integer(1)::i = 1:100 )
      i4(i) = i3(i)             ! trivial (nondestructive) case
   END FORALL
   PRINT *,i3
   PRINT *,i4

   CASENUM = 8
   PRINT *,"CASENUM = ",CASENUM

   DO i = 1,100
      i3(i) = i
   END DO

   FORALL ( integer(1)::i = 1:100 )
      i4(101-i) = i3(i)
   END FORALL
   PRINT *,i3
   PRINT *,i4


   CASENUM = 9
   PRINT *,"CASENUM = ",CASENUM

   DO i = 1,100
      i6(i) = i
   END DO

   FORALL ( integer(1)::i = 1:10, j = 1:10 )
      i5(i,j) = i6( (10-i)*10 + j )
   END FORALL
   PRINT *,i5
   PRINT *,i6

!-------------------------------------
! Test destructive array overlap
! via indirect equivalence
!-------------------------------------

   CASENUM = 10
   PRINT *,"CASENUM = ",CASENUM

   DO i = 1,200
      i9(i) = i
   END DO

   FORALL ( integer(1)::i = 1:100 )
      i8(i) = i9(90+i)
   END FORALL
   PRINT *,i7
   PRINT *,i8
   PRINT *,i9

END PROGRAM
