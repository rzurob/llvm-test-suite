!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : ABS on initializaiton expression
!*
!* DESCRIPTION                : REAL and INTEGER types
!* ===================================================================
       program main
       real(4), parameter :: y4=-2.0E0
       real(8), parameter :: y8=-2.0D0
       real(16), parameter :: y16=-2.0Q0
       real(4) :: x4=abs(y4)
       real(8) :: x8=abs(y8)
       real(16) :: x16=abs(y16)

       integer(1), parameter :: i1=-13_1
       integer(2), parameter :: i2=-13_2
       integer(4), parameter :: i4=-13_4
       integer(8), parameter :: i8=-13_8
       integer(1) :: j1=abs(i1)
       integer(2) :: j2=abs(i2)
       integer(4) :: j4=abs(i4)
       integer(8) :: j8=abs(i8)

       if (x4 /= abs(y4)) stop 1
       if (x8 /= abs(y8)) stop 2
       if (x16 /= abs(y16)) stop 3

       if (j1 /= abs(i1)) stop 4
       if (j2 /= abs(j2)) stop 5
       if (j4 /= abs(j4)) stop 6
       if (j8 /= abs(j8)) stop 7
       end program
