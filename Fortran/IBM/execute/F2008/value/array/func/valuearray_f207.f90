!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2015-09-24
!*
!*  PRIMARY FUNCTIONS TESTED   : VALUE(F2008 extension) - dummy argument arrays allowed with value
!*
!*  DESCRIPTION                : testing the extensions to the VALUE attribute
!*                   for arrays of different types of rank 7 with shape (1,2,3,4,5,6,7)
!*								- passing an array to a function to check that
!*								1. dummy argument is equal to the actual argument
!*								2. actual argument doesn't change
!*								3. function return is as expected
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
integer*4 i1(SZA1,SZA2,SZA3,SZA4,SZA5,SZA6,SZA7),i1_r(SZA1,SZA2,SZA3,SZA4,SZA5,SZA6,SZA7),i1_f(SZA1,SZA2,SZA3,SZA4,SZA5,SZA6,SZA7)
real  r1(SZA1,SZA2,SZA3,SZA4,SZA5,SZA6,SZA7),r1_r(SZA1,SZA2,SZA3,SZA4,SZA5,SZA6,SZA7),r1_f(SZA1,SZA2,SZA3,SZA4,SZA5,SZA6,SZA7)
complex*8  com1(SZA1,SZA2,SZA3,SZA4,SZA5,SZA6,SZA7),com1_r(SZA1,SZA2,SZA3,SZA4,SZA5,SZA6,SZA7),com1_f(SZA1,SZA2,SZA3,SZA4,SZA5,SZA6,SZA7)
character(10) c1(SZA1,SZA2,SZA3,SZA4,SZA5,SZA6,SZA7),c1_r(SZA1,SZA2,SZA3,SZA4,SZA5,SZA6,SZA7),c1_f(SZA1,SZA2,SZA3,SZA4,SZA5,SZA6,SZA7)
logical  l1(SZA1,SZA2,SZA3,SZA4,SZA5,SZA6,SZA7),l1_r(SZA1,SZA2,SZA3,SZA4,SZA5,SZA6,SZA7),l1_f(SZA1,SZA2,SZA3,SZA4,SZA5,SZA6,SZA7)
type(t1) dvt1(SZA1,SZA2,SZA3,SZA4,SZA5,SZA6,SZA7),dvt1_r(SZA1,SZA2,SZA3,SZA4,SZA5,SZA6,SZA7),dvt1_f(SZA1,SZA2,SZA3,SZA4,SZA5,SZA6,SZA7)

i1  =200
i1_r=100

r1  =2*atan(1.0)
r1_r=atan(1.0)

com1  =(2*atan(1.0),3*atan(1.0))
com1_r=(atan(1.0),2*atan(1.0))

c1  = "9v8f3hz38f"
c1_r= "1234567890"

l1  = .false.
l1_r= .true.

dvt1 	= t1(200,2*atan(1.0), .false.,(3*atan(1.0),5*atan(1.0))	,"z8ck29alv3")
dvt1_r 	= t1(100,atan(1.0)	, .true.,(2*atan(1.0),3*atan(1.0))	,"1a3b5c7d9e")


i1_f = func1_int(i1)
if (any (i1_f .ne. i1_r)) error stop 10
if (any (i1 .eq. i1_r)) error stop 210

r1_f = func1_r(r1)
	loop1r: do doc1=1,SZA1
		loop2r: do doc2=1,SZA2
			loop3r: do doc3=1,SZA3
				loop4r: do doc4=1,SZA4
					loop5r: do doc5=1,SZA5
						loop6r: do doc6=1,SZA6
							loop7r: do doc7=1,SZA7
		if (.not. precision_r4(r1_f(doc1,doC2,doc3,doC4,doc5,doC6,doC7),r1_r(doc1,doC2,doc3,doC4,doc5,doC6,doC7))) error stop 11
		if (precision_r4(r1(doc1,doC2,doc3,doC4,doc5,doC6,doC7),r1_r(doc1,doC2,doc3,doC4,doc5,doC6,doC7))) error stop 211
							end do loop7r
						end do loop6r
					end do loop5r
				end do loop4r
			end do loop3r
		end do loop2r
	end do loop1r

com1_f = func1_com(com1)
	loop1c: do doc1=1,SZA1
		loop2c: do doc2=1,SZA2
			loop3c: do doc3=1,SZA3
				loop4c: do doc4=1,SZA4
					loop5c: do doc5=1,SZA5
						loop6c: do doc6=1,SZA6
							loop7c: do doc7=1,SZA7
		if (.not. precision_x8(com1_f(doc1,doC2,doc3,doC4,doc5,doC6,doC7),com1_r(doc1,doC2,doc3,doC4,doc5,doC6,doC7))) error stop 12
		if (precision_x8(com1(doc1,doC2,doc3,doC4,doc5,doC6,doC7),com1_r(doc1,doC2,doc3,doC4,doc5,doC6,doC7))) error stop 212
							end do loop7c
						end do loop6c
					end do loop5c
				end do loop4c
			end do loop3c
		end do loop2c
	end do loop1c

c1_f = func1_char(c1)
if (any (c1_f .ne. c1_r)) error stop 13
if (any (c1 .eq. c1_r)) error stop 213

l1_f=  func1_lg(l1)
if (any (l1_f .NEQV. l1_r)) error stop 14
if (any (l1 .EQV. l1_r)) error stop 214

dvt1_f = func1_dvt(dvt1)
	loop1: do doc1=1,SZA1
		loop2: do doc2=1,SZA2
			loop3: do doc3=1,SZA3
				loop4: do doc4=1,SZA4
					loop5: do doc5=1,SZA5
						loop6: do doc6=1,SZA6
							loop7: do doc7=1,SZA7
	if (dvt1_f(doc1,doC2,doc3,doC4,doc5,doC6,doC7)%i1 		.ne. 	dvt1_r(doc1,doC2,doc3,doC4,doc5,doC6,doC7)%i1) 			error stop 1501
	if (.not. precision_r4 (dvt1_f(doc1,doC2,doc3,doC4,doc5,doC6,doC7)%r1,dvt1_r(doc1,doC2,doc3,doC4,doc5,doC6,doC7)%r1)) 	error stop 1502
	if (dvt1_f(doc1,doC2,doc3,doC4,doc5,doC6,doC7)%l1 		.NEQV. 	dvt1_r(doc1,doC2,doc3,doC4,doc5,doC6,doC7)%l1) 			error stop 1503
	if (.not. precision_x8 (dvt1_f(doc1,doC2,doc3,doC4,doc5,doC6,doC7)%c1,dvt1_r(doc1,doC2,doc3,doC4,doc5,doC6,doC7)%c1)) 	error stop 1504
	if (dvt1_f(doc1,doC2,doc3,doC4,doc5,doC6,doC7)%char1 	.ne. 	dvt1_r(doc1,doC2,doc3,doC4,doc5,doC6,doC7)%char1) 		error stop 1505

	if (dvt1(doc1,doC2,doc3,doC4,doc5,doC6,doC7)%i1 		.eq. 	dvt1_r(doc1,doC2,doc3,doC4,doc5,doC6,doC7)%i1) 		error stop 2501
	if (precision_r4 (dvt1(doc1,doC2,doc3,doC4,doc5,doC6,doC7)%r1,dvt1_r(doc1,doC2,doc3,doC4,doc5,doC6,doC7)%r1)) 	error stop 2502
	if (dvt1(doc1,doC2,doc3,doC4,doc5,doC6,doC7)%l1 		.EQV. 	dvt1_r(doc1,doC2,doc3,doC4,doc5,doC6,doC7)%l1) 		error stop 2503
	if (precision_x8 (dvt1(doc1,doC2,doc3,doC4,doc5,doC6,doC7)%c1,dvt1_r(doc1,doC2,doc3,doC4,doc5,doC6,doC7)%c1)) 	error stop 2504
	if (dvt1(doc1,doC2,doc3,doC4,doc5,doC6,doC7)%char1 	.eq. 	dvt1_r(doc1,doC2,doc3,doC4,doc5,doC6,doC7)%char1) 	error stop 2505
							end do loop7
						end do loop6
					end do loop5
				end do loop4
			end do loop3
		end do loop2
	end do loop1

contains

integer*4 function func1_int(arg)
    integer*4 :: arg(:,:,:,:,:,:,:)
	value arg
	if (any (arg .ne. i1)) error stop 110
	arg = 100
	func1_int = 100
end

real function func1_r(arg)
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
	arg =atan(1.0)
	func1_r=atan(1.0)
end

complex*8 function func1_com(arg)
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
	arg =(atan(1.0),2*atan(1.0))
	func1_com=(atan(1.0),2*atan(1.0))
end

character(10) function func1_char(arg)
    character(10) :: arg(:,:,:,:,:,:,:)
	value arg
	if (any (arg .ne. c1)) error stop 113
	arg = "1234567890"
	func1_char = "1234567890"
end

logical function func1_lg(arg)
    logical :: arg(:,:,:,:,:,:,:)
	value arg
	if (any (arg .NEQV. l1)) error stop 114
	arg = .true.
	func1_lg = .true.
end

type(t1) function func1_dvt(arg)
    type(t1) :: arg(:,:,:,:,:,:,:)
	value arg
	loop1: do doc1=1,SZA1
		loop2: do doc2=1,SZA2
			loop3: do doc3=1,SZA3
				loop4: do doc4=1,SZA4
					loop5: do doc5=1,SZA5
						loop6: do doc6=1,SZA6
							loop7: do doc7=1,SZA7
		if (arg(doc1,doC2,doc3,doC4,doc5,doC6,doC7)%i1 		.ne. 	dvt1(doc1,doC2,doc3,doC4,doc5,doC6,doC7)%i1) 		error stop 11501
		if (.not. precision_r4 (arg(doc1,doC2,doc3,doC4,doc5,doC6,doC7)%r1,dvt1(doc1,doC2,doc3,doC4,doc5,doC6,doC7)%r1)) 	error stop 11502
		if (arg(doc1,doC2,doc3,doC4,doc5,doC6,doC7)%l1 		.NEQV. 	dvt1(doc1,doC2,doc3,doC4,doc5,doC6,doC7)%l1) 		error stop 11503
		if (.not. precision_x8 (arg(doc1,doC2,doc3,doC4,doc5,doC6,doC7)%c1,dvt1(doc1,doC2,doc3,doC4,doc5,doC6,doC7)%c1)) 	error stop 11504
		if (arg(doc1,doC2,doc3,doC4,doc5,doC6,doC7)%char1 	.ne. 	dvt1(doc1,doC2,doc3,doC4,doc5,doC6,doC7)%char1) 		error stop 11505
							end do loop7
						end do loop6
					end do loop5
				end do loop4
			end do loop3
		end do loop2
	end do loop1
	arg	= t1(100,atan(1.0)	, .true.,(2*atan(1.0),3*atan(1.0)),"1a3b5c7d9e")
	func1_dvt= t1(100,atan(1.0)	, .true.,(2*atan(1.0),3*atan(1.0)),"1a3b5c7d9e")
end

end
