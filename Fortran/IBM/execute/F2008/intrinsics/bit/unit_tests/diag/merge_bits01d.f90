!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : merge-bits01d.f
!*
!*  PROGRAMMER                 : Maryam Moghadas
!*  DATE                       : 2013-02-27
!*  ORIGIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : MERGE_BITS intrinsics
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : MERGE_BITS(I, J, MASK)
!*
!*  Test the compilation fails if:
!*
!*  * first argument  not an integer or a boz literal constant, 
!*  * second argument not an integer or a boz literal constant,
!*  * first and second arguments are both integer but with different kind parameter, 
!*  * first and second arguments  are both  boz literal constants, 
!*  * third argument not an integer or a boz literal constant, or an integer with different 
!*    kind parameter than any other integer argument 
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================

program maskl_maskr

 real :: i_r = 3, j_r = 5, mask_r = 2
 
 integer (KIND = 2) :: i1 = 3, j1 = 5
 
 integer (KIND = 8) :: i2 = 3, j2 = 5
 data i3 /B"00110011"/
 data j3 /B"11001100"/

!---- MERGE_BITS should not accept any type rather than integer or boz literal for its first and second arg ----!
 print *, 'MERGE_BITS(i_r, 5, 2)  ', MERGE_BITS(i_r, 5, 2)
 print *, 'MERGE_BITS(3, j_r, 2)  ', MERGE_BITS(3, j_r, 2)
 print *, 'MERGE_BITS(i_r, j_r, 2)  ', MERGE_BITS(i_r, j_r, 2)

!---- MERGE_BITS should not accept that first and second arguments to be integers if different kinds ----!
 print *, 'MERGE_BITS(3, j1, 2)', MERGE_BITS(3, j1, 2)
 print *, 'MERGE_BITS(i1, 5, 2)', MERGE_BITS(i1, 5, 2) 
 print *, 'MERGE_BITS(i2, j1, 2)', MERGE_BITS(i2, j1, 2)

!---- MERGE_BITS should not accept two boz literal numbers as its first and second arguments ----!
 print *, 'MERGE_BITS(B"00110011", B"11001100", 2)', MERGE_BITS(B"00110011", B"11001100", 2)

!---- MERGE_BITS should not accept any third argument rather than boz literal or integer with the same kind as----! 
                         !-- any other integer argument --!
 
 print *, 'MERGE_BITS(i1, j1, mask_r)', MERGE_BITS(i1, j1, mask_r)
 print *, 'MERGE_BITS(i1, j1, 2)', MERGE_BITS(i1, j1, 2)
 print *, 'MEREGE_BITS(INT(2,1), INT(3,1), INT(4,8))', MERGE_BITS(INT(2,1), INT(3,1), INT(4,8))
end 



























