!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : cobound_d008.f
!*
!*  PROGRAMMER                 : Francesco Cassullo
!*  DATE                       : September 2010
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Return type testing of lcobound/ucobound
!*                               with 2/3 arguments returns a scalar integer
!*                               of specified kind.
!*                            
!234567890123456789012345678901234567890123456789012345678901234567890

program main
	
	integer, save :: caf[*]

	integer*1 :: i1, i1arr(1)
	integer*2 :: i2, i2arr(1)
	integer*4 :: i4, i4arr(1)
	integer*8 :: i8, i8arr(1)
	real*8 :: r8, r8arr(1)
	real*4 :: r4, r4arr(1)
	complex(4) :: cx4, cx4arr(1)
	complex(8) :: cx8, cx8arr(1)
	character(9) :: ch
	logical*1 :: l1, l1arr(1)
	logical*2 :: l2, l2arr(1)
	logical*4 :: l4, l4arr(1)
	logical*8 :: l8, l8arr(1)
	   
	
!### lcobound
	!!!Valid
	i1 = lcobound(caf, 1, 2)
	i2 = lcobound(caf, 1, 4)
	i4 = lcobound(caf, 1, 8)
	i8 = lcobound(caf, 1, 1)

	r4 = lcobound(caf, 1, 1)
	r8 = lcobound(caf, 1, 2)
	cx4 = lcobound(caf, 1, 4)
	cx8 = lcobound(caf, 1, 8)
	
	i1arr = lcobound(COARRAY=caf, KIND=8)
	i2arr = lcobound(COARRAY=caf, KIND=4)
	i4arr = lcobound(COARRAY=caf, KIND=2)
	i8arr = lcobound(COARRAY=caf, KIND=1)
	r8arr = lcobound(COARRAY=caf, KIND=2)
	r4arr = lcobound(COARRAY=caf, KIND=2)
	cx4arr = lcobound(COARRAY=caf, KIND=1)
	cx8arr = lcobound(COARRAY=caf, KIND=1)
	
	!!!!Invalid
	l1 = lcobound(caf, 1, 2)
	l2 = lcobound(caf, 1, 1)
	l4 = lcobound(caf, 1, 8)
	l8 = lcobound(caf, 1, 4)
	ch = lcobound(caf, 1, 1)	
	l1arr = lcobound(COARRAY=caf, KIND=2)
	l2arr = lcobound(COARRAY=caf, KIND=4)
	l4arr = lcobound(COARRAY=caf, KIND=8)
	l8arr = lcobound(COARRAY=caf, KIND=1)
	
	
!### ucobound
	!!!Valid
	i1 = ucobound(caf, 1, 8)
	i2 = ucobound(caf, 1, 1)
	i4 = ucobound(caf, 1, 2)
	i8 = ucobound(caf, 1, 4)

	r4 = ucobound(caf, 1, 8)
	r8 = ucobound(caf, 1, 4)
	cx4 = ucobound(caf, 1, 2)
	cx8 = ucobound(caf, 1, 1)
	
	i1arr = ucobound(COARRAY=caf, KIND=8)
	i2arr = ucobound(COARRAY=caf, KIND=4)
	i4arr = ucobound(COARRAY=caf, KIND=2)
	i8arr = ucobound(COARRAY=caf, KIND=1)
	r8arr = ucobound(COARRAY=caf, KIND=2)
	r4arr = ucobound(COARRAY=caf, KIND=2)
	cx4arr = ucobound(COARRAY=caf, KIND=1)
	cx8arr = ucobound(COARRAY=caf, KIND=1)
	
	!!!Invalid
	l1 = ucobound(caf, 1, 8)
	l2 = ucobound(caf, 1, 1)
	l4 = ucobound(caf, 1, 2)
	ch = ucobound(caf, 1, 1)
	l1arr = ucobound(COARRAY=caf, KIND=8)
	l2arr = ucobound(COARRAY=caf, KIND=4)
	l4arr = ucobound(COARRAY=caf, KIND=2)
	l8arr = ucobound(COARRAY=caf, KIND=1)
	
end


logical*8 function fun1()
	integer, save :: caf[*]
	
	fun1 = lcobound(caf, 1, 1)
end function