! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2013-02-10
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   : MERGE_BITS intrinsic
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MERGE_BITS(I,J,MASK)
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

 PROGRAM MERGE_BITS_TEST
 IMPLICIT NONE
 INTEGER :: a=13, b=18, mask=22, arr_a(2), arr_b(2), arr_mask(2)
 INTEGER (KIND=2) :: b1=5, mask1
 INTEGER :: i = MERGE_BITS(13,18,22)
 DATA mask1 /B"10101010"/
 arr_a = (/5,6/)
 arr_b = (/10,9/)
 arr_mask = (/41,40/)

!---- testing different methods for passing arguments into this intrinsic ----!
 IF (MERGE_BITS(13,18,22) .NE. 4) ERROR STOP 1
!--- using argument keyword to reorder the arguments ----
 IF (MERGE_BITS(J=18,MASK=22,I=13) .NE. 4) ERROR STOP 2

 IF (MERGE_BITS(13,b,mask) .NE. 4) ERROR STOP 3
 IF (MERGE_BITS(a,b,mask) .NE. 4) ERROR STOP 4

!---- testing binary-constant as an argument-----!
 IF (MERGE_BITS(B"00001101",b,mask) .NE. 4) ERROR STOP 5
 IF (MERGE_BITS(13,B"00010010",mask) .NE. 4) ERROR STOP 6
 IF (MERGE_BITS(a,b,B"00010110") .NE. 4) ERROR STOP 7

!---- testing octal-constant as an argument -----!
 IF (MERGE_BITS(O"00000015",b,mask) .NE. 4) ERROR STOP 8
 IF (MERGE_BITS(a,O"00000022",mask) .NE. 4) ERROR STOP 9
 IF (MERGE_BITS(a,b,O"00000026") .NE. 4) ERROR STOP 10

!---- testing hex-constant as an argument ------!
 IF (MERGE_BITS(Z"0000000D",b,mask) .NE. 4) ERROR STOP 11
 IF (MERGE_BITS(a,Z"00000012",mask) .NE. 4) ERROR STOP 12
 IF (MERGE_BITS(a,b,Z"00000016") .NE. 4) ERROR STOP 13

!---- passing two boz literal constants as argument ----!
 IF (MERGE_BITS(B"00001101",b,B"00010110") .NE. 4) ERROR STOP 14

!---- passing argument with different kind parameters ---!
 !---KIND =1
 IF (MERGE_BITS(INT(B"00110011",1),B"11110000",B"10101010") .NE. 114) ERROR STOP 15
 IF (MERGE_BITS(INT(51,1), B"11110000", B"10101010") .NE. 114) ERROR STOP 16
 !--- KIND = 2
 IF (MERGE_BITS(b1,INT(10,2), o'51') .NE. 3) ERROR STOP 17
 !--- KIND = 8
 IF (MERGE_BITS(INT(18,8), INT(10,8), B"00101001") .NE. 2) ERROR STOP 18

!--- testing when intrinsics appear in a constant expression ----!
 IF (MERGE_BITS(i+1 , 10 , 41) .NE. 3) ERROR STOP 19

!--- applying MERGE_BITS on array arguments ---------------------!

 IF (ANY(MERGE_BITS(arr_a, 10 ,41) .NE. (/3,2/))) ERROR STOP 20

 IF (ANY(MERGE_BITS(arr_a, arr_b, 41) .NE. (/MERGE_BITS(arr_a(1),arr_b(1),41), MERGE_BITS(arr_a(2), arr_b(2), 41) /)) ) ERROR STOP 21

 IF (ANY(MERGE_BITS(arr_a, arr_b, arr_mask) .NE. (/MERGE_BITS(arr_a(1), arr_b(1), arr_mask(1)) , MERGE_BITS(arr_a(2), arr_b(2), arr_mask(2)) /) )) ERROR STOP 22

 PRINT *, MERGE_BITS(arr_a, 10, 41)
 PRINT *, "End of the program: Normal termination"

End



