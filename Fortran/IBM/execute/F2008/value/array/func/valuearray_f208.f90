!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2015-09-24
!*
!*  PRIMARY FUNCTIONS TESTED   : VALUE(F2008 extension) - dummy argument arrays allowed with value
!*
!*  DESCRIPTION                : testing the extensions to the VALUE attribute
!*                                for passing an array of different types as actual argument
!*								-subroutines are declared as PURE
!*								testing will check that
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

integer SIZEOFA, doCounter
parameter (SIZEOFA = 10)

logical, external :: precision_x8, precision_r4
integer doc1,doC2,doc3,doC4,doc5,doC6,doC7
integer SZA1,SZA2,SZA3,SZA4,SZA5,SZA6,SZA7
parameter (SZA1 = 1)
parameter (SZA2 = 2)
parameter (SZA3 = 3)
parameter (SZA4 = 4)
parameter (SZA5 = 5)
parameter (SZA6 = 6)
parameter (SZA7 = 7)
integer*4 i2(SZA1,SZA2,SZA3,SZA4,SZA5,SZA6,SZA7),i2_r(SZA1,SZA2,SZA3,SZA4,SZA5,SZA6,SZA7)
integer*4 i3(SZA7,SZA6,SZA5,SZA4,SZA3,SZA2,SZA1),i3_r(SZA7,SZA6,SZA5,SZA4,SZA3,SZA2,SZA1)
integer*4 i4(SZA5,SZA4,SZA7,SZA6,SZA3,SZA1,SZA2),i4_r(SZA5,SZA4,SZA7,SZA6,SZA3,SZA1,SZA2)
integer*4 i5(SZA5,SZA7,SZA4,SZA6,SZA7),i5_r(SZA5,SZA7,SZA4,SZA6,SZA7)

integer*4 i1(10),i1_r(10)
real  r1(10),r1_r(10)
complex*8  com1(10),com1_r(10)
character(SIZEOFA) c1(10),c1_r(10)
logical  l1(10),l1_r(10)
type(t1) dvt1(10),dvt1_r(10)

loop1a: do doc1=1,SZA1
	loop2a: do doc2=1,SZA2
		loop3a: do doc3=1,SZA3
			loop4a: do doc4=1,SZA4
				loop5a: do doc5=1,SZA5
					loop6a: do doc6=1,SZA6
						loop7a: do doc7=1,SZA7

	i2	(doc1,doc2,doc3,doc4,doc5,doc6,doc7)= doc1+doc2+doc3+doc4+doc5+doc6+doc7
	i2_r(doc1,doc2,doc3,doc4,doc5,doc6,doc7)= doc1+doc2+doc3+doc4+doc5+doc6+doc7

						end do loop7a
					end do loop6a
				end do loop5a
			end do loop4a
		end do loop3a
	end do loop2a
end do loop1a

loop1b: do doc1=1,SZA7
	loop2b: do doc2=1,SZA6
		loop3b: do doc3=1,SZA5
			loop4b: do doc4=1,SZA4
				loop5b: do doc5=1,SZA3
					loop6b: do doc6=1,SZA2
						loop7b: do doc7=1,SZA1

	i3	(doc1,doc2,doc3,doc4,doc5,doc6,doc7)= doc1+doc2+doc3+doc4+doc5+doc6+doc7
	i3_r(doc1,doc2,doc3,doc4,doc5,doc6,doc7)= doc1+doc2+doc3+doc4+doc5+doc6+doc7

						end do loop7b
					end do loop6b
				end do loop5b
			end do loop4b
		end do loop3b
	end do loop2b
end do loop1b

loop1c: do doc1=1,SZA5
	loop2c: do doc2=1,SZA4
		loop3c: do doc3=1,SZA7
			loop4c: do doc4=1,SZA6
				loop5c: do doc5=1,SZA3
					loop6c: do doc6=1,SZA1
						loop7c: do doc7=1,SZA2

	i4	(doc1,doc2,doc3,doc4,doc5,doc6,doc7)= doc1+doc2+doc3+doc4+doc5+doc6+doc7
	i4_r(doc1,doc2,doc3,doc4,doc5,doc6,doc7)= doc1+doc2+doc3+doc4+doc5+doc6+doc7

						end do loop7c
					end do loop6c
				end do loop5c
			end do loop4c
		end do loop3c
	end do loop2c
end do loop1c

loop1d: do doc1=1,SZA5
	loop2d: do doc2=1,SZA7
		loop3d: do doc3=1,SZA4
			loop4d: do doc4=1,SZA6
				loop5d: do doc5=1,SZA7

	i5	(doc1,doc2,doc3,doc4,doc5)= doc1+doc2+doc3+doc4+doc5
	i5_r(doc1,doc2,doc3,doc4,doc5)= doc1+doc2+doc3+doc4+doc5

				end do loop5d
			end do loop4d
		end do loop3d
	end do loop2d
end do loop1d

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
do doCounter=1,SIZEOFA
	if (.not. precision_r4(r1(doCounter),r1_r(doCounter))) error stop 11
end do

call sub1_com(com1)
do doCounter=1,SIZEOFA
	if (.not. precision_x8(com1(doCounter),com1_r(doCounter))) error stop 12
end do

call sub1_char(c1)
if (any (c1 .ne. c1_r)) error stop 13

call sub1_lg(l1)
if (any (l1 .NEQV. l1_r)) error stop 14

call sub1_dvt(dvt1)
do doCounter=1,SIZEOFA
	if (dvt1(doCounter)%i1 		.ne. 	dvt1_r(doCounter)%i1) 			error stop 1501
	if (.not. precision_r4 (dvt1(doCounter)%r1,dvt1_r(doCounter)%r1)) 	error stop 1502
	if (dvt1(doCounter)%l1 		.NEQV. 	dvt1_r(doCounter)%l1) 			error stop 1503
	if (.not. precision_x8 (dvt1(doCounter)%c1,dvt1_r(doCounter)%c1)) 	error stop 1504
	if (dvt1(doCounter)%char1 	.ne. 	dvt1_r(doCounter)%char1) 		error stop 1505
end do

call sub2_int(i2)
if (any (i2 .ne. i2_r)) error stop 16

call sub2_int(i3)
if (any (i3 .ne. i3_r)) error stop 17

call sub2_int(i4)
if (any (i4 .ne. i4_r)) error stop 18

call sub3_int(i5)
if (any (i5 .ne. i5_r)) error stop 19

contains

pure subroutine sub1_int(arg)
    integer*4 :: arg(:)
	value arg
	arg = 200
end subroutine

pure subroutine sub2_int(arg)
    integer*4 :: arg(:,:,:,:,:,:,:)
	value arg
	arg = 200
end subroutine

pure subroutine sub3_int(arg)
    integer*4 :: arg(:,:,:,:,:)
	value arg
	arg = 200
end subroutine

pure subroutine sub1_r(arg)
    real :: arg(:)
	value arg
	arg=4*atan(1.0)
end subroutine

pure subroutine sub1_com(arg)
    complex*8 :: arg(:)
	value arg
	arg=(5*atan(1.0),7*atan(1.0))
end subroutine

pure subroutine sub1_char(arg)
    character(SIZEOFA) :: arg(:)
	value arg
	arg = "abcdefghij"
end subroutine

pure subroutine sub1_lg(arg)
    logical :: arg(:)
	value arg
	arg = .false.
end subroutine

pure subroutine sub1_dvt(arg)
    type(t1) :: arg(:)
	value arg
	arg	= t1(400,4*atan(1.0),.false.,(3*atan(1.0),7*atan(1.0)),"6pq94jv382")
end subroutine

end
