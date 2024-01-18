!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrTransferIntLog.f 
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
!* - data-pointer & target have diff kind parameter
!* -  apply -qintsize/qrealsize option 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

@process intsize(2)
@process realsize(8)

    program main

        integer*2, pointer :: ip(:)
        integer, target, allocatable :: it (:)

	logical*2, target :: lt(8)
	logical, pointer :: lp(:)

 	real*16, target :: rt(5) = (/ (real(i,kind=kind(0.0)*2), i = 11,15 ) /)
 	double precision, pointer :: rp(:) 

        allocate(it(10), source=(/ (i, i=1,10) /) )

        ip(kind(it):) => it(::kind(ip))
        if ( .not. associated(ip)) stop 11

	print *, lbound(ip,1), ubound(ip,1)
	print *, ip
	print *, transfer(ip, (/1/) )

	lt = mod(it(2:9),2) == 1

	lp(sum(ip):) => lt
        if ( .not. associated(lp,lt)) stop 21 

	print *, lbound(lp,1), ubound(lp,1)
	print *, lp 
	print *, transfer(lp, (/.false./)  )


	rp(kind(2):kind(1*2.0)/2) => rt(::len('ok'))
        if ( .not. associated(rp)) stop 21 
	print *, lbound(rp,1), ubound(rp,1)

	write (*, '(3f20.15)') rp
	write (*, '(3f20.15)') transfer(rp, 1.0_16) 
    end program
