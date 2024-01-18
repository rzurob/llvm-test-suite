!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrFuncNameAsRemap1.f
!*
!*  PROGRAMMER                 : Michelle Zhang 
!*  DATE                       : Aug 31, 2006
!*  ORIGIN                     : Compiler Development, IBM Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement 
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION
!*  
!*  - This is diagnostic testcase
!*  The name of the module array function with pointer attribute is used 
!*  as bounds_remapping of data-pointer-object. 
!*  defect 324114
!*
!234567890123456789012345678901234567890123456789012345678901234567890

		character(:), target, allocatable :: t_ch1(:)
		character(:), pointer :: p_ch1(:,:,:)

		allocate( t_ch1(20), source = (/ (achar(i),i=65,84) /) )  

		p_ch1( func('ab'), 1:2,1:2) => t_ch1
		
		contains
	            function func(ch)
			character(len=2) :: ch
			integer, pointer :: func(:)
			
			if ( ch == 'ab') allocate(func(2),source=(/1,2/))
		    end function 

	        end
