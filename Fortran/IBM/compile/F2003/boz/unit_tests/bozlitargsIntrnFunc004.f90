! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug. 26, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : boz-literal args to REAL, INT, CMPLX and DBLE intrinsics
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  : -qlanglvl=2003std
!*
!*  DESCRIPTION                : using boz-literals in various situations to genereate
!*															 the appropriate language level message for Fortran 2003
!*
!234567890123456789012345678901234567890123456789012345678901234567890
integer :: i,k
real :: j,a,x
logical :: l
complex :: m
real, dimension(3) :: b

!Array Constructor
b=(/ 10, z'1', z'2' /)

!Assignment
i=z'15a'

!exponentiation
i=z'15a'**z'2'

!Logical Operations
l=.not.z'15a'
l= b'1'.neqv.o'2'

!Addition, Subtraction, Multiplication, Division
i=i+z'15a'
i=z'15a'-i
i=i*z'15a'
i=i/z'15a'

!Intrinsics
j=abs(z'15a')
j=dim(10,z'15a')

!Kind Parameter for REAL, INT, CMPLX intrinsics.
i=int(z'15a',o'10')
j=real(z'15a',b'100')
m=cmplx(z'15a',z'15a',z'4')

!do loops
do i=z'1',z'10'
j=j
end do

!if statements
if(z'15a' /= i) then
j=j
end if

!I/O statements
OPEN(UNIT=z'7', FILE='temp.txt')
READ(UNIT=z'7', FMT='(F8.6)') A
READ (z'7', FMT = '(F8.6)') X
BACKSPACE (z'10', ERR = 20)
WRITE (o'7', *) 2, 3
close(b'0111')

!forall statements
forall (i=z'1':z'a':z'1')
k=10
end forall

!subroutine calls
call test(z'15a',z'10a')
end program

subroutine test(x1,x2)
integer :: x1, x2
x1=z'5a'
x2=z'10a'
end subroutine test

