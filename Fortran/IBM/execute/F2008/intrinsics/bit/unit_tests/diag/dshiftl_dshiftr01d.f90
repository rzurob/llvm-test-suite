!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2013-02-27
!*
!*  PRIMARY FUNCTIONS TESTED   : DSHIFTL, DSHIFTR intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : DSHIFTL(I, J, SHIFT), DSHIFTR(I, J, SHIFT)
!*
!*  Test the compilation fails if:
!*
!*  * first argument not an integer or a boz literal constant,
!*  * second argument not an integer or a boz literal,
!*  * first and second arguments are integer but with different kinds,
!*  * first and second arguments are both boz literal constants,
!*  * third argument not a nonnegative integer or greater than bit_size(I or J)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================

program dshiftl_dshiftr

 real :: i_r = 3, j_r = 4, shift_r = 5
 integer (KIND = 2 ) :: i1 = 3
 integer (KIND = 4) :: j1 = 4

 data i2 /B"00010011"/
 data j2 /B"00110000"/

 !---- DSHIFTL/R should not accept real numbers as its first/second argument ----!
  print *,'DSHIFTL(i_r, 4, 5)  ', DSHIFTL(i_r, 4, 5)
  print *,'DSHIFTL(3, j_r, 5)  ', DSHIFTL(3, j_r, 5)

  print *, 'DSHIFTR(i_r, 4, 5)  ', DSHIFTR(i_r, 4, 5)
  print *, 'DSHIFTR(3, j_r, 5)  ', DSHIFTR(3, j_r, 5)

  print *, 'DSHIFTL(i_r, j_r, 5)  ', DSHIFTL(i_r, j_r, 5)
  print *, 'DSHIFTR(i_r, j_r, 5)  ', DSHIFTR(i_r, j_r, 5)

 !---- DSHIFTL/R should not accept boz literal numbers as both its first and second argument ----!
  print *, 'DSHIFTL(B"00110011", B"01010101", 5)  ', DSHIFTL(B"00110011", B"01010101", 5)
  print *, 'DSHIFTR(B"00110011", B"01010101", 5)  ', DSHIFTR(B"00110011", B"01010101", 5)

  print *, 'DSHIFTL(i2, j2, 5)  ', DSHIFTL(i2, 4_1, 5)
  print *, 'DSHIFTR(i2, j2, 5)  ', DSHIFTR(i2, 4_1, 5)

!---- DSHIFTL/R shuold not accept two integers of different kinds as its first and second arguments ----!
  print *, 'DSHIFTL(INT(3,1), INT(4,2), 5)  ', DSHIFTL(INT(3,1), INT(4,2), 5)
  print *, 'DSHIFTR(INT(3,1), INT(4,2), 5)  ', DSHIFTR(INT(3,1), INT(4,2), 5)

 !---- DSHIFTL/R should not accept any third argument rather than nonnegative integer ----!
             ! less then or equal to bit_size(I) or bit_size(j) !

  print *, 'DSHIFTL(3, 4, shift_r)  ', DSHIFTL(3, 4, shift_r)
  print *, 'DSHIFTR(3, 4, shift_r)  ', DSHIFTR(3, 4, shift_r)

  print *, 'DSHIFTL(3_1, 4_1, 9)  ', DSHIFTL(3_1, 4_1, 9)
  print *, 'DSHIFTR(3_1, 4_1, 9)  ', DSHIFTR(3_1, 4_1, 9)

  print *, 'DSHIFTL(3_2, 4_2, 17)  ', DSHIFTL(3_2, 4_2, 17)
  print *, 'DSHIFTR(3_2, 4_2, 17)  ', DSHIFTR(3_2, 4_2, 17)

  print *, 'DSHIFTL(3, 4, 35)  ', DSHIFTL(3, 4, 35)
  print *, 'DSHIFTR(3, 4, 35)  ', DSHIFTR(3, 4, 35)

  print *, 'DSHIFTL(3_4, 4_4, 35)  ', DSHIFTL(3_4, 4_4, 35)
  print *, 'DSHIFTR(3_4, 4_4, 35)  ', DSHIFTR(3_4, 4_4, 35)

  print *, 'DSHIFTL(3_8, 4_8, 65)  ', DSHIFTL(3_8, 4_8, 65)
  print *, 'DSHIFTR(3_8, 4_8, 65)  ', DSHIFTR(3_8, 4_8, 65)

  print *, 'DSHIFTL(3, 4, -2)  ', DSHIFTL(3, 4, -2)
  print *, 'DSHIFTR(3, 4, -2)  ', DSHIFTR(3, 4, -2)
end

