! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/fxieee.presh fxieee13
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : F2K IEEE Modules
!*
!*  PROGRAMMER                 : Marcus Yu
!*  DATE                       : February 7, 2002
!*  ORIGIN                     : XL Fortran Development
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_SELECTED_REAL_KIND
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf90
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*
!234567890123456789012345678901234567890123456789012345678901234567890
	     program fxieee13

        use ieee_arithmetic
        use constants_for_ieee

        integer :: xi, yi, zi, i, j
	
	    do i = 0, 6
		   do j = 0, 37
		      zi = ieee_selected_real_kind(i, j)
              if (zi /= 4 ) print *, "ieee_selected_real_kind 4 failed at ", i, j
		   enddo
		enddo
		
	    do i = 0, 6
		   do j = 38, 307
		      zi = ieee_selected_real_kind(i, j)
              if (zi /= 8 ) print *, "ieee_selected_real_kind 8 failed at ", i, j
		   enddo
		enddo   

        do i = 7, 15
		   do j = 0, 307
		      zi = ieee_selected_real_kind(i, j)
              if (zi /= 8 ) print *, "ieee_selected_real_kind 8 failed at ", i, j
		   enddo
		enddo 

        do i = 16, 31
		   do j = 0, 291
		      zi = ieee_selected_real_kind(i, j)
              if (zi /= 16 ) print *, "ieee_selected_real_kind 16 failed at ", i, j
		   enddo
		 enddo 
	     
		 xi = 32
		 yi = 37
		 zi = ieee_selected_real_kind(xi, yi)
		 if (zi /= -1 ) print *, "ieee_selected_real_kind failed.", zi
	   	 
		 xi = 0
		 yi = 308
 		 zi = ieee_selected_real_kind(xi, yi)
         if (zi /= -2 ) print *, "ieee_selected_real_kind failed."

		 xi = 32
		 yi = 308
		 zi = ieee_selected_real_kind(xi, yi)
		 if (zi /= -3 ) print *, "ieee_selected_real_kind failed."
		 
		 end program

