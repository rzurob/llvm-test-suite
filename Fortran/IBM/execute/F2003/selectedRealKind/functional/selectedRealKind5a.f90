!***********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : selectedRealKind5a
!*  TEST CASE TITLE            : 
!*
!*
!*  PROGRAMMER                 : Salma Elshatanoufy
!*  DATE                       : 08/24/2007
!*  ORIGIN                     : XL Fortran Compiler Development, IBM Torolab
!*
!*  PRIMARY FUNCTIONS TESTED   : -4 return value from SELECTED_REAL_KIND([P, R])
!*				 intrinsic
!*  SECONDARY FUNCTIONS TESTED : see below
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : The testcase is testing the -4 return
!*				 case from selected_real_kind, when the
!*				 PRECISION and RANGE intrinsics are used.
!*				 
!*
!*
!234567890123456789012345678901234567890123456789012345678901234567890

	program selectedRealKind5a
		real(8) r
		real(16) f
		integer, dimension(3) :: ans
		parameter (ans = (/ -4, -4, -4/))
	   	integer, dimension(3) :: res
	   	parameter ( res = [ 	selected_real_kind (P = precision(r)+1    ,R =  range(f)+1 ),     		      &
					selected_real_kind (P = (precision(r)*2)-8, R= range(r)-((range(r)- range(f))/2)),    &
					selected_real_kind (P = (range(f)-7)/15   , R = (precision(f)-1)*10)  ] )
	
	 do i = 1, 3
        	  if (res(i) .ne. ans(i)) call zzrc(i)
       	 end do

       end program

