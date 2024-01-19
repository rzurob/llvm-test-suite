! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2013-02-10
!*
!*  PRIMARY FUNCTIONS TESTED   : DSHIFTL, DSHIFTR intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : DSHIFTL/DSHIFTR(I,J,SHIFT)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

PROGRAM DSHIFTL_DSHIFTR
   IMPLICIT NONE
   INTEGER (KIND = 1) :: i = 69, j = 9
   INTEGER :: i1=1, j1=16, shift1=3, shift = 2
   INTEGER :: i2=1, j2=2**30, shift2=2
   INTEGER :: expr_2 = DSHIFTL(1,10,2), expr_1 = DSHIFTR(2**21,15,1)
   INTEGER :: t, n(20)
   INTEGER :: arr_i1(2), arr_j1(2), arr_i2(2), arr_j2(2), arr_shift(2)
   INTEGER :: arr_res2(2), arr_res22(2), arr_res1(2), arr_res11(2)
   INTEGER :: arr_test(2)
   DO t = 1,20
     n(t) = 2**i
   END DO

   arr_i1(1) = 2**28
   arr_i1(2) = 2**22

   arr_i2(1) = 1
   arr_i2(2) = 2

   arr_j1(1) = 12
   arr_j1(2) = 8

   arr_j2(1) = 2**30
   arr_j2(2) = 2**25

   arr_shift(1) = 2
   arr_shift(2) = 3

   arr_res2(1) = 5
   arr_res2(2) = 9

   arr_res22(1) = 5
   arr_res22(2) = 16

   arr_res11(1) = 3
   arr_res11(2) = 1

 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 ! according to standard2008, DSHIFTL(1,2**30,2)=5      !
 ! Also, for any i : DSHIFTL(i,i,shift)= ISHFTC(i,shift)!
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   IF ( DSHIFTL(1,2**30,2) .NE. 5) ERROR STOP 1
   IF ( DSHIFTL(1,j2,shift2)  .NE. 5) ERROR STOP 2
   IF ( DSHIFTL(i2,2**30,shift2) .NE. 5) ERROR STOP 3
   IF ( DSHIFTL(i2,j2,2) .NE. 5) ERROR STOP 4
   IF ( DSHIFTL(1,j2,2) .NE. 5) ERROR STOP 5
   IF ( DSHIFTL(1,2**30,shift2) .NE. 5) ERROR STOP 6
   IF ( DSHIFTL(i2, 2**30,2) .NE. 5) ERROR STOP 7

   IF ( DSHIFTL(i2,j2,shift2) .NE. 5) ERROR STOP 8
   IF ( DSHIFTL(i2,i2,shift2) .NE. ISHFTC(i2,shift2)) ERROR STOP 9
   IF ( DSHIFTL(j2,j2,shift2) .NE. ISHFTC(j2,shift2)) ERROR STOP 10

 !----testing DSHIFTL with passing boz litteral constants as argument---!
   IF ( DSHIFTL(69_1, 9_1, 2) .NE. DSHIFTL(INT(B"01000101", 1), B"00001001", 2) ) ERROR STOP 50
   IF ( DSHIFTL(69_1, 9_1, 2) .NE. DSHIFTL( 69_1, B"00001001", 2) ) ERROR STOP 51
   IF ( DSHIFTL(69_1, 9_1, 2) .NE. DSHIFTL(i, j, shift) ) ERROR STOP 52

 !--------------- passing argument of different kind ------------------!
   IF ( DSHIFTL(69_1, 9_1, 2) .NE. IOR(SHIFTL(69_1,2), SHIFTR(9_1,8-2)) ) ERROR STOP 60
   IF ( DSHIFTL(69_2, 9_2, 2) .NE. IOR(SHIFTL(69_2,2), SHIFTR(9_2,16-2)) ) ERROR STOP 61
   IF ( DSHIFTL(69_4, 9_4, 2) .NE. IOR(SHIFTL(69_4,2), SHIFTR(9_4,32-2)) ) ERROR STOP 62
   IF ( DSHIFTL(69_8, 9_8, 2) .NE. IOR(SHIFTL(69_8,2), SHIFTR(9_8,64-2)) ) ERROR STOP 63

!----------- testing DSHIFTL with array argument ----------------------!
   IF (ANY(DSHIFTL(arr_i2, j2, shift2) .NE. arr_res2) ) ERROR STOP 11
   IF (ANY(DSHIFTL(arr_i2, arr_j2, arr_shift) .NE. arr_res22)) ERROR STOP 12
   PRINT *,DSHIFTL(arr_i2, j2, shift2)

!---------- testing using argument keywords ---------------------------!
   IF (DSHIFTL(J = 2**30, SHIFT = 2, I = 1)  .NE. 5) ERROR STOP 13

 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 ! according to standard2008, DSHIFTR(1,16,3)= 2**29 +2 !
 ! also for any i: DSHIFTR(i,i,shift) = ISHFTC(i,-shift)!
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   IF ( DSHIFTR(1, 16, 3) .NE. 536870914) ERROR STOP 14
   IF ( DSHIFTR(i1 ,16, 3) .NE. DSHIFTR(1, 16, 3) ) ERROR STOP 15
   IF ( DSHIFTR(1, j1, shift1) .NE. DSHIFTR(1, 16, 3)) ERROR STOP 16
   IF ( DSHIFTR(i1, j1, 3) .NE. DSHIFTR(1, 16, 3)) ERROR STOP 17
   IF ( DSHIFTR(1, 16, shift1) .NE. DSHIFTR(1, 16, 3)) ERROR STOP 18
   IF ( DSHIFTR(1, j1, 3) .NE. DSHIFTR(1, 16, 3)) ERROR STOP 19
   IF ( DSHIFTR(i1, 16, 3) .NE. DSHIFTR(1, 16, 3)) ERROR STOP 20

   IF ( DSHIFTR(i1,j1,shift1) .NE. 536870914) ERROR STOP 21
   IF ( DSHIFTR(i1,i1,shift1) .NE. ISHFTC(i1,-shift1)) ERROR STOP 22
   IF ( DSHIFTR(j1,j1,shift1) .NE. ISHFTC(j1,-shift1)) ERROR STOP 23
 !---------------- passing argument of different kind -------------------!
   IF ( DSHIFTR(69_1, 9_1, 2) .NE. IOR(SHIFTL(69_1,8-2), SHIFTR(9_1,2)) ) ERROR STOP 70
   IF ( DSHIFTR(69_2, 9_2, 2) .NE. IOR(SHIFTL(69_2,16-2), SHIFTR(9_2, 2)) ) ERROR STOP 71
   IF ( DSHIFTR(69_4, 9_4, 2) .NE. IOR(SHIFTL(69_4,32-2), SHIFTR(9_4,2)) ) ERROR STOP 72
   IF ( DSHIFTR(69_8, 9_8, 2) .NE. IOR(SHIFTL(69_8,64-2), SHIFTR(9_8,2)) ) ERROR STOP 73

 !--- testing DSHIFTR with passing boz litteral constants as argument- --!
   IF ( DSHIFTR(69_1, 9_1, 2) .NE. DSHIFTR(INT(B"01000101", 1), B"00001001", 2) ) ERROR STOP 80
   IF ( DSHIFTR(69_1, 9_1, 2) .NE. DSHIFTR(69_1, B"00001001", 2) ) ERROR STOP 81
   IF ( DSHIFTR(69_1, 9_1, 2) .NE. DSHIFTR(i, j, shift) ) ERROR STOP 82

 !------------- testing DSHIFTR with array trgument ---------------------!
   IF (ANY(DSHIFTR(arr_i1, arr_j1, arr_shift) .NE. arr_res11)) ERROR STOP 24
   PRINT *,DSHIFTR(arr_i1, j2, 2)

 !------------- testing using argumetn keywords -------------------------!
   IF ( DSHIFTR(J = 16, SHIFT = 3, I = 1) .NE. DSHIFTR(1, 16, 3)) ERROR STOP 25

 !--- testing when DSHIFTL and DSHIFTR apears in constant expression ----!
   IF ( (expr_1 .NE. 7) .AND. (expr_2 .NE. 4) ) ERROR STOP 26

   print*, "End of the program: Normal termination"

 END


