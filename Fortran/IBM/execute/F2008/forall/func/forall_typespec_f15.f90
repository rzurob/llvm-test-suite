!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2012-06-25
!*
!*  PRIMARY FUNCTIONS TESTED   : FORALL with type specifier (F2008 extension)
!*  SECONDARY FUNCTIONS TESTED : Use of FORALL constructs/statements in
!*                               subroutines
!*  ADAPTED FROM               : hpf_forall/construct/fxfc034b.scenario
!*
!*  DESCRIPTION
!*
!*    Test the FORALL-construct with assignments to adjustable-shape arrays.
!*    Test with local dummy arrays distributed onto local processor
!*    arrangements.
!*
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM fxfc034b

   INTEGER CASENUM,in1,in2,in3

   integer,parameter :: p1=-1,p2=3

   INTEGER i1(100)
   INTEGER i2(-55:55)
   INTEGER i3(1000:1099)
   INTEGER i4(20,20)
   INTEGER i5(26,23)
   INTEGER i6(0:25,-11:11)
   INTEGER i7(20,20)
   INTEGER i8(20,20)
   INTEGER i9(-10:9,10:29)
   INTEGER i10(20,20)

   INTEGER i11(20)
   INTEGER i12(0:61)
   INTEGER i13(20)
   INTEGER i14(20,20)
   INTEGER,PARAMETER :: i = -26,j = -1, k = 1, l = 23
   INTEGER i15(-50:0,0:50)
   INTEGER i16(-20:20,999:1011)
   INTEGER i17(-40:-7,-18:10)
   INTEGER i18(-50:0,20)
   INTEGER i19(20,20)
   INTEGER i20(-11:11,-11:11)

   INTEGER i21(80)
   INTEGER i22(-51:51)
   INTEGER i23(120)
   INTEGER i24(40,-60:-1)
   INTEGER i25(53,72)
   INTEGER i27(20,-20:20)
   INTEGER i28(40,20)
   INTEGER i29(140,180)
   INTEGER i30(60,20)

   interface

      SUBROUTINE sub1( array,n )
         INTEGER n,array(n)
      END SUBROUTINE

      SUBROUTINE sub2( array,n )
         INTEGER n,array(-10:n)
      END SUBROUTINE

      SUBROUTINE sub3( array,n )
         INTEGER n,array(n)
      END SUBROUTINE

      SUBROUTINE sub4( array,m,n )
         INTEGER m,n,array(m,n)
      END SUBROUTINE

      SUBROUTINE sub4b( array,m,n )
         INTEGER m,n,array(m,n)
      END SUBROUTINE

      SUBROUTINE sub5( array,m,n )
         INTEGER m,n,array(-5:m,-2:n)
      END SUBROUTINE

      SUBROUTINE sub6( array,m,n )
         INTEGER m,n,array(-5:m,-2:n)
      END SUBROUTINE

      SUBROUTINE sub7( array,m,n )
         INTEGER m,n,array(m,n)
      END SUBROUTINE

      SUBROUTINE sub8( array,m,n )
         INTEGER m,n,array(m,n)
      END SUBROUTINE

      SUBROUTINE sub9( array,m,n )
         INTEGER m,n,array(m,n)
      END SUBROUTINE

      SUBROUTINE sub10( array,m,n )
         INTEGER m,n,array(m,n)
         INTEGER in1,in2
      END SUBROUTINE

   end interface

!==================================================================
! Test assignment to whole arrays
!==================================================================

   CASENUM = 1
   PRINT *,"CASENUM=",CASENUM
   i1 = 0
   CALL sub1( i1,100 )
   PRINT *,i1

   CASENUM = 2
   PRINT *,"CASENUM=",CASENUM
   i2 = 0
   CALL sub2( i2,100 )
   PRINT *,i2

   CASENUM = 3
   PRINT *,"CASENUM=",CASENUM
   i3 = 0
   CALL sub3( i3,100 )
   PRINT *,i3

   CASENUM = 4
   PRINT *,"CASENUM=",CASENUM
   i4 = 0
   CALL sub4( i4,20,20 )
   PRINT *,i4

   CASENUM = 5
   PRINT *,"CASENUM=",CASENUM
   i5 = 0
   CALL sub5( i5,20,20 )
   PRINT *,i5

   CASENUM = 6
   PRINT *,"CASENUM=",CASENUM
   i6 = 0
   CALL sub6( i6,20,20 )
   PRINT *,i6

   CASENUM = 7
   PRINT *,"CASENUM=",CASENUM
   i7 = 0
   CALL sub7( i7,20,20 )
   PRINT *,i7

   CASENUM = 8
   PRINT *,"CASENUM=",CASENUM
   i8 = 0
   CALL sub8( i8,20,20 )
   PRINT *,i8

   CASENUM = 9
   PRINT *,"CASENUM=",CASENUM
   i9 = 0
   CALL sub9( i9,20,20 )
   PRINT *,i9

   CASENUM = 10
   PRINT *,"CASENUM=",CASENUM
   i10 = 0
   CALL sub10( i10,20,20 )
   PRINT *,i10

!==================================================================
! Test assignment to array sections
!==================================================================

   CASENUM = 11
   PRINT *,"CASENUM=",CASENUM
   i11 = 0
   CALL sub1( i11(:),20 )
   PRINT *,i11

   CASENUM = 12
   PRINT *,"CASENUM=",CASENUM
   i12 = 0
   CALL sub2( i12(::2),20 )
   PRINT *,i12

   CASENUM = 13
   PRINT *,"CASENUM=",CASENUM
   i13 = 0
   CALL sub3( i13(ubound(i13,1):1:-1),20 )
   PRINT *,i13

   CASENUM = 14
   PRINT *,"CASENUM=",CASENUM
   i14 = 0
   CALL sub4( i14(:,:),20,20 )
   PRINT *,i14

   CASENUM = 15
   PRINT *,"CASENUM=",CASENUM
   i15 = 0
   CALL sub5( i15(i:j,k:l),20,20 )
   PRINT *,i15

   CASENUM = 16
   PRINT *,"CASENUM=",CASENUM
   i16 = 0
   CALL sub6( i16(-10:10,1000:1010),15,8 )
   PRINT *,i16

   CASENUM = 17
   PRINT *,"CASENUM=",CASENUM
   i17 = 0
   CALL sub7( i17(i:,:j),20,18 )
   PRINT *,i17

   CASENUM = 18
   PRINT *,"CASENUM=",CASENUM
   i18 = 0
   CALL sub8( i18(i:j,:),26,20 )
   PRINT *,i18

   CASENUM = 19
   PRINT *,"CASENUM=",CASENUM
   i19 = 0
   CALL sub9( i19(:,:),20,20 )
   PRINT *,i19

   CASENUM = 20
   PRINT *,"CASENUM=",CASENUM
   i20 = 0
   CALL sub10( i20(-10:10,-10:10),21,21 )
   PRINT *,i20

!==================================================================
! Test assignment to array sections with stride
!==================================================================

   CASENUM = 21
   PRINT *,"CASENUM=",CASENUM
   i21 = 0
   CALL sub1( i21(::2),40 )
   PRINT *,i21

   CASENUM = 22
   PRINT *,"CASENUM=",CASENUM
   i22 = 0
   in1 = -51
   in2 = 51
   in3 = 2
   CALL sub2( i22(in1:in2:in3),41 )
   PRINT *,i22

   CASENUM = 23
   PRINT *,"CASENUM=",CASENUM
   i23 = 0
   CALL sub3( i23(ubound(i23,1):1:-3),40 )
   PRINT *,i23

   CASENUM = 24
   PRINT *,"CASENUM=",CASENUM
   i24 = 0
   CALL sub4( i24(::2,::3),20,20 )
   PRINT *,i24

   CASENUM = 25
   PRINT *,"CASENUM=",CASENUM
   i25 = 0
   CALL sub5( i25(2::2,3::3),20,20 )
   PRINT *,i25

   CASENUM = 27
   PRINT *,"CASENUM=",CASENUM
   i27 = 0
   CALL sub7( i27(:,::2),20,21 )
   PRINT *,i27

   CASENUM = 28
   PRINT *,"CASENUM=",CASENUM
   i28 = 0
   CALL sub8( i28(::2,:),20,20 )
   PRINT *,i28

   CASENUM = 29
   PRINT *,"CASENUM=",CASENUM
   i29 = 0
   CALL sub9( i29(::7,::9),20,20 )
   PRINT *,i29(::7,::9)

   CASENUM = 30
   PRINT *,"CASENUM=",CASENUM
   i30 = 0
   CALL sub10( i30(ubound(i30,1):lbound(i30,1):-3,:),20,20 )
   PRINT *,i30

END PROGRAM

!==================================================================

!-------------------------------
! This subroutine accepts a one
! dimensional array and assigns
! incrementing values to it.
!
! Test: assigment-stmt
!
!-------------------------------

SUBROUTINE sub1( array,n )
   INTEGER n,array(n)
   INTEGER in1

   FORALL ( integer(1):: in1 = 1:n )
      array(in1) = in1
   END FORALL
END SUBROUTINE

!-------------------------------
! This subroutine accepts a one
! dimensional array and assigns
! values to it.
!
! Test: forall-stmt
!
!-------------------------------

SUBROUTINE sub2( array,n )
   INTEGER n,array(-10:n)
   INTEGER in1,in2

   FORALL ( integer(2)::in1 = -10:n )
      array(in1) = in1
      forall ( in2 = in1:in1 ) array(in2) = array(in1) + in2
   END FORALL
END SUBROUTINE


!-------------------------------
! This subroutine accepts a one
! dimensional array and assigns
! values to it.
!
! Test: FORALL-construct
!
!-------------------------------

SUBROUTINE sub3( array,n )
   INTEGER n,array(n)
   INTEGER in1,in2

   FORALL ( integer(4)::in1 = 4:n:4 )
      FORALL ( integer::in2 = 3:0:-2 )
         array(in1-in2:in1-in2+1) = in1+in2
      END FORALL
   END FORALL
END SUBROUTINE

!-------------------------------
! This subroutine accepts a two
! dimensional array and assigns
! values to it.
!
! Test: assignment-stmt
!
!-------------------------------

SUBROUTINE sub4( array,m,n )
   INTEGER m,n,array(m,n)
   INTEGER in1,in2

   FORALL ( integer(4)::in1 = 1:m, in2 = 1:n )
      array(in1,in2) = in1+in2
   END FORALL
END SUBROUTINE

!-------------------------------
! This subroutine accepts a two
! dimensional array and assigns
! values to it.
!
! Test: array assignment-stmt
!
!-------------------------------

SUBROUTINE sub4b( array,m,n )
   INTEGER m,n,array(m,n)
   INTEGER in1

   FORALL (integer(2):: in1 = 1:m )
      array(in1,:) = in1
   END FORALL
END SUBROUTINE

!-------------------------------
! This subroutine accepts a two
! dimensional array and assigns
! values to it.
!
! Test: where-stmt
!
!-------------------------------

SUBROUTINE sub5( array,m,n )
   INTEGER m,n,array(-5:m,-2:n)
   INTEGER in1,in2

   forall ( integer(4)::in1 = -5:m, in2 = -2:n ) array(in1,in2) = in1+in2

   FORALL ( integer::in1 = -5:m )
      where ( array(in1,:) .GT. 0 ) array(in1,:) = -99
   END FORALL
END SUBROUTINE

!-------------------------------
! This subroutine accepts a two
! dimensional array and assigns
! values to it.
!
! Test: WHERE-construct
!
!-------------------------------

SUBROUTINE sub6( array,m,n )
   INTEGER m,n,array(-5:m,-2:n)
   INTEGER in1,in2

   FORALL ( integer(4)::in1 = -5:m )
      forall (integer(2)::in2 = -2:n ) array(in1,in2) = in1 + in2
      WHERE ( array(in1,::2) .GT. 0 )
         array(in1,::2) = -99
         array(in1,n:-2:-2) = -98
      END WHERE
   END FORALL
END SUBROUTINE

!-------------------------------
! This subroutine accepts a two
! dimensional array and assigns
! values to it.
!
! Test: forall-stmt
!
!-------------------------------

SUBROUTINE sub7( array,m,n )
   INTEGER m,n,array(m,n)
   INTEGER in1,in2

   FORALL ( integer(1)::in1 = 1:m )
      forall ( integer(2)::in2 = 1:n ) array(in1,in2) = in1 + in2
      forall ( integer(4)::in2 = 1:n,in1 .EQ. in2) array(in1,in2) = -99
      forall ( integer::in2 = 2:n:2 ) array(in1,in2) = array(in1,in2-1)
   END FORALL
END SUBROUTINE

!-------------------------------
! This subroutine accepts a two
! dimensional array and assigns
! values to it.
!
! Test: array forall-stmt
!
!-------------------------------

SUBROUTINE sub8( array,m,n )
   INTEGER m,n,array(m,n)
   INTEGER in1,in2

   FORALL ( integer(4)::in1 = 1:m )
      forall ( integer(2)::in2 = 3:n:3 ) &
         array(in1,in2-2:in2) = (/ in1,in2,in1+in2 /)
   END FORALL
END SUBROUTINE

!-------------------------------
! This subroutine accepts a two
! dimensional array and assigns
! values to it.
!
! Test: FORALL-construct
!
!-------------------------------

SUBROUTINE sub9( array,m,n )
   INTEGER m,n,array(m,n)
   INTEGER in1,in2

   FORALL ( integer(1)::in1 = 1:m )
      FORALL ( integer(2)::in2 = 2:n:2 )
         array(in1,in2-1) = in1 + in2 -1
         array(in1,in2)   = in1 + in2
      END FORALL
      FORALL ( integer(4):: in2 = 1:n, in1 .EQ. in2 )
         array(in1,in2) = -99
      END FORALL
   END FORALL
END SUBROUTINE

!-------------------------------
! This subroutine accepts a two
! dimensional array and assigns
! values to it.
!
! Test: array FORALL-construct
!
!-------------------------------

SUBROUTINE sub10( array,m,n )
   INTEGER m,n,array(m,n),array2(m,n)
   INTEGER in1,in2

   FORALL ( integer::in1 = 1:m )
      FORALL ( integer(4)::in2 = 1:n:n )
         array(in1,in2) = in1
         array(in1,in2+1::2) = array(in1,in2:n-1:2)
         array(in1,in2) = array(in1,in2) + 1
      END FORALL
   END FORALL
END SUBROUTINE

