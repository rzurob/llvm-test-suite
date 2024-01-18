!*  ===================================================================
!*
!*  DATE                       : June 2, 2009
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray in data stmt
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  : -qcaf
!*
!*  DESCRIPTION                :
!*
!*     C565: A data-stmt-object or data-i-do-object shall not be a coindexed variable.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

   !Currently we disallows [] in data. i.e., We flag systax errors
   !When C565 implemeted, the vf needs to change accordingly.

   integer, save :: s_coarr[*]
   integer, save :: arr_coarr(3)[*]

   data s_coarr/1/
   data s_coarr[1]/1/
   data (s_coarr[i], i=1,1)/1/
   data (s_coarr, i=1,coarr[1])/1/

   data arr_coarr(1)/1/
   data arr_coarr(1)[1]/1/
   data (arr_coarr(i)[i], i=1,1)/1/
   data (arr_coarr(i), i=1,coarr(1)[1])/1/

   end

