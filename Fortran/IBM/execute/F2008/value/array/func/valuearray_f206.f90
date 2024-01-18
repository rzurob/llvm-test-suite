!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : F2008/value/array/func/valuearray_f206.f
!*
!*  PROGRAMMER                 : Cezar Lutac 
!*  DATE                       : 2015-09-24
!*
!*  PRIMARY FUNCTIONS TESTED   : VALUE(F2008 extension) - dummy argument arrays allowed with value
!*
!*  DESCRIPTION                : testing the extensions to the VALUE attribute
!*                   for arrays of different types of rank 7 with shape (1,2,3,4,5,6,7)
!*								- passing an array to a subroutine to check that
!*								1. dummy argument is equal to the actual argument
!*								2. actual argument doesn't change
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

implicit none

type t1
  integer i1
  real r1
  logical l1
  complex c1
  character(10) char1
end type

integer doc1,doC2,doc3,doC4,doc5,doC6,doC7
integer SZA1,SZA2,SZA3,SZA4,SZA5,SZA6,SZA7
parameter (SZA1 = 1)
parameter (SZA2 = 2)
parameter (SZA3 = 3)
parameter (SZA4 = 4)
parameter (SZA5 = 5)
parameter (SZA6 = 6)
parameter (SZA7 = 7)

logical, external :: precision_x8, precision_r4
integer*4 i1(SZA1,SZA2,SZA3,SZA4,SZA5,SZA6,SZA7),i1_r(SZA1,SZA2,SZA3,SZA4,SZA5,SZA6,SZA7)
real  r1(SZA1,SZA2,SZA3,SZA4,SZA5,SZA6,SZA7),r1_r(SZA1,SZA2,SZA3,SZA4,SZA5,SZA6,SZA7)
complex*8  com1(SZA1,SZA2,SZA3,SZA4,SZA5,SZA6,SZA7),com1_r(SZA1,SZA2,SZA3,SZA4,SZA5,SZA6,SZA7)
character(10) c1(SZA1,SZA2,SZA3,SZA4,SZA5,SZA6,SZA7),c1_r(SZA1,SZA2,SZA3,SZA4,SZA5,SZA6,SZA7)
logical  l1(SZA1,SZA2,SZA3,SZA4,SZA5,SZA6,SZA7),l1_r(SZA1,SZA2,SZA3,SZA4,SZA5,SZA6,SZA7)
type(t1) dvt1(SZA1,SZA2,SZA3,SZA4,SZA5,SZA6,SZA7),dvt1_r(SZA1,SZA2,SZA3,SZA4,SZA5,SZA6,SZA7)

i1  =100
i1_r=100

r1  =atan(1.0)
r1_r=atan(1.0)

com1  =(atan(1.0),2*atan(1.0))
com1_r=(atan(1.0),2*atan(1.0))

c1  = "1234567890"
c1_r= "1234567890"

l1  = .true.
l1_r= .true.

dvt1 	= t1(100,atan(1.0)	, .true.,(2*atan(1.0),3*atan(1.0)),"1a3b5c7d9e")
dvt1_r 	= t1(100,atan(1.0)	, .true.,(2*atan(1.0),3*atan(1.0)),"1a3b5c7d9e")


call sub1_int(i1)
if (any (i1 .ne. i1_r)) error stop 10

call sub1_r(r1)
	loop1r: do doc1=1,SZA1
		loop2r: do doc2=1,SZA2
			loop3r: do doc3=1,SZA3
				loop4r: do doc4=1,SZA4
					loop5r: do doc5=1,SZA5
						loop6r: do doc6=1,SZA6
							loop7r: do doc7=1,SZA7
		if (.not. precision_r4(r1(doc1,doC2,doc3,doC4,doc5,doC6,doC7),r1_r(doc1,doC2,doc3,doC4,doc5,doC6,doC7))) error stop 11
							end do loop7r
						end do loop6r
					end do loop5r
				end do loop4r
			end do loop3r
		end do loop2r
	end do loop1r

call sub1_com(com1)
	loop1c: do doc1=1,SZA1
		loop2c: do doc2=1,SZA2
			loop3c: do doc3=1,SZA3
				loop4c: do doc4=1,SZA4
					loop5c: do doc5=1,SZA5
						loop6c: do doc6=1,SZA6
							loop7c: do doc7=1,SZA7
			if (.not. precision_x8(com1(doc1,doC2,doc3,doC4,doc5,doC6,doC7),com1_r(doc1,doC2,doc3,doC4,doc5,doC6,doC7))) error stop 12
							end do loop7c
						end do loop6c
					end do loop5c
				end do loop4c
			end do loop3c
		end do loop2c
	end do loop1c

call sub1_char(c1)
if (any (c1 .ne. c1_r)) error stop 13

call sub1_lg(l1)
if (any (l1 .NEQV. l1_r)) error stop 14

call sub1_dvt(dvt1)
	loop1: do doc1=1,SZA1
		loop2: do doc2=1,SZA2
			loop3: do doc3=1,SZA3
				loop4: do doc4=1,SZA4
					loop5: do doc5=1,SZA5
						loop6: do doc6=1,SZA6
							loop7: do doc7=1,SZA7
			if (dvt1(doc1,doC2,doc3,doC4,doc5,doC6,doC7)%i1 		.ne. 	dvt1_r(doc1,doC2,doc3,doC4,doc5,doC6,doC7)%i1) 			error stop 1501
			if (.not. precision_r4 (dvt1(doc1,doC2,doc3,doC4,doc5,doC6,doC7)%r1,dvt1_r(doc1,doC2,doc3,doC4,doc5,doC6,doC7)%r1)) 	error stop 1502
			if (dvt1(doc1,doC2,doc3,doC4,doc5,doC6,doC7)%l1 		.NEQV. 	dvt1_r(doc1,doC2,doc3,doC4,doc5,doC6,doC7)%l1) 			error stop 1503	
			if (.not. precision_x8 (dvt1(doc1,doC2,doc3,doC4,doc5,doC6,doC7)%c1,dvt1_r(doc1,doC2,doc3,doC4,doc5,doC6,doC7)%c1)) 	error stop 1504
			if (dvt1(doc1,doC2,doc3,doC4,doc5,doC6,doC7)%char1 	.ne. 	dvt1_r(doc1,doC2,doc3,doC4,doc5,doC6,doC7)%char1) 		error stop 1505
							end do loop7
						end do loop6
					end do loop5
				end do loop4
			end do loop3
		end do loop2
	end do loop1

contains
  
subroutine sub1_int(arg)
    integer*4 :: arg(:,:,:,:,:,:,:)
	value arg
	if (any (arg .ne. i1)) error stop 110
	
	if (size(arg) .ne. SZA1*SZA2*SZA3*SZA4*SZA5*SZA6*SZA7) 				error stop 101
	if ( any(lbound(arg) .ne. (/1,1,1,1,1,1,1/))) 						error stop 102
	if ( any(ubound(arg) .ne. (/SZA1,SZA2,SZA3,SZA4,SZA5,SZA6,SZA7/))) 	error stop 103
	if (rank(arg) .ne. 7) 												error stop 104
	if (any(shape(arg) .ne. (/SZA1,SZA2,SZA3,SZA4,SZA5,SZA6,SZA7/))) 	error stop 105

	arg = 200	
end subroutine

subroutine sub1_r(arg)
    real :: arg(:,:,:,:,:,:,:)
	value arg
	loop1: do doc1=1,SZA1
		loop2: do doc2=1,SZA2
			loop3: do doc3=1,SZA3
				loop4: do doc4=1,SZA4
					loop5: do doc5=1,SZA5
						loop6: do doc6=1,SZA6
							loop7: do doc7=1,SZA7
			if (.not. precision_r4(arg(doc1,doC2,doc3,doC4,doc5,doC6,doC7),r1(doc1,doC2,doc3,doC4,doc5,doC6,doC7))) error stop 111
							end do loop7
						end do loop6
					end do loop5
				end do loop4
			end do loop3
		end do loop2
	end do loop1
	if (size(arg) .ne. SZA1*SZA2*SZA3*SZA4*SZA5*SZA6*SZA7) 				error stop 201
	if ( any(lbound(arg) .ne. (/1,1,1,1,1,1,1/))) 						error stop 202
	if ( any(ubound(arg) .ne. (/SZA1,SZA2,SZA3,SZA4,SZA5,SZA6,SZA7/))) 	error stop 203
	if (rank(arg) .ne. 7) 												error stop 204
	if (any(shape(arg) .ne. (/SZA1,SZA2,SZA3,SZA4,SZA5,SZA6,SZA7/))) 	error stop 205	
	arg=4*atan(1.0)
end subroutine

subroutine sub1_com(arg)
    complex*8 :: arg(:,:,:,:,:,:,:)
	value arg
	loop1: do doc1=1,SZA1
		loop2: do doc2=1,SZA2
			loop3: do doc3=1,SZA3
				loop4: do doc4=1,SZA4
					loop5: do doc5=1,SZA5
						loop6: do doc6=1,SZA6
							loop7: do doc7=1,SZA7
			if (.not. precision_x8(arg(doc1,doC2,doc3,doC4,doc5,doC6,doC7),com1(doc1,doC2,doc3,doC4,doc5,doC6,doC7))) error stop 112
							end do loop7
						end do loop6
					end do loop5
				end do loop4
			end do loop3
		end do loop2
	end do loop1
	if (size(arg) .ne. SZA1*SZA2*SZA3*SZA4*SZA5*SZA6*SZA7) 				error stop 301
	if ( any(lbound(arg) .ne. (/1,1,1,1,1,1,1/))) 						error stop 302
	if ( any(ubound(arg) .ne. (/SZA1,SZA2,SZA3,SZA4,SZA5,SZA6,SZA7/))) 	error stop 303
	if (rank(arg) .ne. 7) 												error stop 304
	if (any(shape(arg) .ne. (/SZA1,SZA2,SZA3,SZA4,SZA5,SZA6,SZA7/))) 	error stop 305	
	arg=(5*atan(1.0),7*atan(1.0))
end subroutine	

subroutine sub1_char(arg)
    character(10) :: arg(:,:,:,:,:,:,:)
	value arg
	if (any (arg .ne. c1)) error stop 113	
	
	if (size(arg) .ne. SZA1*SZA2*SZA3*SZA4*SZA5*SZA6*SZA7) 				error stop 401
	if ( any(lbound(arg) .ne. (/1,1,1,1,1,1,1/))) 						error stop 402
	if ( any(ubound(arg) .ne. (/SZA1,SZA2,SZA3,SZA4,SZA5,SZA6,SZA7/))) 	error stop 403
	if (rank(arg) .ne. 7) 												error stop 404
	if (any(shape(arg) .ne. (/SZA1,SZA2,SZA3,SZA4,SZA5,SZA6,SZA7/))) 	error stop 405	
	arg = "abcdefghij"	
end subroutine

subroutine sub1_lg(arg)
    logical :: arg(:,:,:,:,:,:,:)
	value arg
	if (any (arg .NEQV. l1)) error stop 114

	if (size(arg) .ne. SZA1*SZA2*SZA3*SZA4*SZA5*SZA6*SZA7) 				error stop 501
	if ( any(lbound(arg) .ne. (/1,1,1,1,1,1,1/))) 						error stop 502
	if ( any(ubound(arg) .ne. (/SZA1,SZA2,SZA3,SZA4,SZA5,SZA6,SZA7/))) 	error stop 503
	if (rank(arg) .ne. 7) 												error stop 504
	if (any(shape(arg) .ne. (/SZA1,SZA2,SZA3,SZA4,SZA5,SZA6,SZA7/))) 	error stop 505	
	arg = .false.
end subroutine	

subroutine sub1_dvt(arg)	
    type(t1) :: arg(:,:,:,:,:,:,:)
	value arg
	loop1: do doc1=1,SZA1
		loop2: do doc2=1,SZA2
			loop3: do doc3=1,SZA3
				loop4: do doc4=1,SZA4
					loop5: do doc5=1,SZA5
						loop6: do doc6=1,SZA6
							loop7: do doc7=1,SZA7
			if (arg(doc1,doC2,doc3,doC4,doc5,doC6,doC7)%i1 		.ne. 	dvt1(doc1,doC2,doc3,doC4,doc5,doC6,doC7)%i1) 			error stop 11501
			if (.not. precision_r4 (arg(doc1,doC2,doc3,doC4,doc5,doC6,doC7)%r1,dvt1(doc1,doC2,doc3,doC4,doc5,doC6,doC7)%r1)) 	error stop 11502
			if (arg(doc1,doC2,doc3,doC4,doc5,doC6,doC7)%l1 		.NEQV. 	dvt1(doc1,doC2,doc3,doC4,doc5,doC6,doC7)%l1) 			error stop 11503	
			if (.not. precision_x8 (arg(doc1,doC2,doc3,doC4,doc5,doC6,doC7)%c1,dvt1(doc1,doC2,doc3,doC4,doc5,doC6,doC7)%c1)) 	error stop 11504
			if (arg(doc1,doC2,doc3,doC4,doc5,doC6,doC7)%char1 	.ne. 	dvt1(doc1,doC2,doc3,doC4,doc5,doC6,doC7)%char1) 		error stop 11505
							end do loop7
						end do loop6
					end do loop5
				end do loop4
			end do loop3
		end do loop2
	end do loop1
	if (size(arg) .ne. SZA1*SZA2*SZA3*SZA4*SZA5*SZA6*SZA7) 				error stop 601
	if ( any(lbound(arg) .ne. (/1,1,1,1,1,1,1/))) 						error stop 602
	if ( any(ubound(arg) .ne. (/SZA1,SZA2,SZA3,SZA4,SZA5,SZA6,SZA7/))) 	error stop 603
	if (rank(arg) .ne. 7) 												error stop 604
	if (any(shape(arg) .ne. (/SZA1,SZA2,SZA3,SZA4,SZA5,SZA6,SZA7/))) 	error stop 605	
	arg	= t1(400,4*atan(1.0),.false.,(3*atan(1.0),7*atan(1.0)),"6pq94jv382")
end subroutine

end
