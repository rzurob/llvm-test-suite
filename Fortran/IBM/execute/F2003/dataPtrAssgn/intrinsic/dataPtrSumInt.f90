!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrSumInt.f 
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
!*  DESCRIPTION   use data-pointer as args of intrinsic SUM;
!*		  data-pointer of type integer;
!*		  data-target is array;
!*	  	  data_pointer with bounds_spec-list ;
!*  
!234567890123456789012345678901234567890123456789012345678901234567890

	program main
            integer, parameter :: I1=3, I2=4 
	    integer*8, target :: t_int(I1,I2,I1)
	    integer*8, pointer :: p_int(:,:,:)
	    integer*8, pointer :: p1_int(:,:)

	    logical, pointer :: lp(:,:,:)
	    logical precision_x8

            allocate(lp(I1,I2,I1), source=.false.)

	    do i = 1, I1
	        do j = 1, I2
		   if ( i==j) lp(i,j,:) = .true. 
	        enddo
            enddo

	    do i = 1, I1 
		t_int(i,:,:) = i + 2
		do j = 1, I2
	            t_int(i,j,:) = i + j 
		enddo
	    enddo

	    p_int(-1:,0:,I2-I1:) => t_int 
	
	    if ( .not. associated(p_int, t_int) ) stop 5
	    if ( any (lbound(p_int) .ne. (/-1,0,1 /))) stop 7	
	    if ( any (ubound(p_int) .ne. (/1,3,3 /))) stop 9 

	    print *, sum(p_int, 1) 
	    print *, sum(p_int, 2, lp) 
	    print *, sum(p_int, lp) 

 	    allocate(p1_int(3,3), source = sum(t_int,2, lp) )
	    p1_int(I1-I2:, I1+I2:) => p1_int(:,:)
	
	    if ( any (lbound(p1_int) .ne. (/-1,7 /))) stop 11 
	    if ( any (ubound(p1_int) .ne. (/1,9 /))) stop 13 

	    print *, sum(p1_int, 2)

	End program
