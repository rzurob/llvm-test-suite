!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : cobound_d006.f
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
!*                               with 1 argument returns a rank-1 array.
!*                            
!234567890123456789012345678901234567890123456789012345678901234567890

program main
	
	integer, save :: caf[*]
	integer :: iscal
	real :: rscal
	complex :: cscal
	logical :: lscal

	integer*1 :: i1(1), ia(2)
	integer*2 :: i2(1)
	integer*4 :: i4(1)
	integer*8 :: i8(1), ib(3)
	real*8 :: r8(1), rc(4)
	real*4 :: r4(1)
	complex(4) :: cx4(1), cd(5)
	complex(8) :: cx8(1)
	character(3) :: ch(1)
	logical*1 :: l1(1)
	logical*2 :: l2(1)
	logical*4 :: l4(1)
	logical*8 :: l8(1)
	   
	
!### lcobound
	i1 = lcobound(caf)
	i2 = lcobound(caf)
	i4 = lcobound(caf)
	i8 = lcobound(caf)

	r4 = lcobound(caf)
	r8 = lcobound(caf)
	cx4 = lcobound(caf)
	cx8 = lcobound(caf)
	
	l1 = lcobound(caf)
	l2 = lcobound(caf)
	l4 = lcobound(caf)
	l8 = lcobound(caf)
	ch = lcobound(caf)
	
	iscal = lcobound(caf)
	rscal = lcobound(caf)
	cscal = lcobound(caf)
	lscal = lcobound(caf)
	caf = lcobound(caf)
	ia = lcobound(caf)
	rc = lcobound(caf)
	

!### ucobound	
	i1 = ucobound(caf)
	i2 = ucobound(caf)
	i4 = ucobound(caf)
	i8 = ucobound(caf)

	r4 = ucobound(caf)
	r8 = ucobound(caf)
	cx4 = ucobound(caf)
	cx8 = ucobound(caf)
	
	l1 = ucobound(caf)
	l2 = ucobound(caf)
	l4 = ucobound(caf)
	l8 = ucobound(caf)
	ch = ucobound(caf)
	
	iscal = ucobound(caf)
	rscal = ucobound(caf)
	cscal = ucobound(caf)
	lscal = ucobound(caf)
	caf = ucobound(caf)
	ib = ucobound(caf)
	cd = ucobound(caf)
	
end
