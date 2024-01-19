!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : ABS on initializaiton expression
!*
!* DESCRIPTION                : langlvl message
!* ===================================================================
       program main
       real :: x=abs(-2.0)
       real(4) :: x4=abs(1.0_4)
       real(8) :: x8=abs(1.0_8)
       real(16) :: x16=abs(1.0Q0)

       integer :: j=abs(-2)
       integer(1) :: j1=abs(-1_1)
       integer(2) :: j2=abs(-1_2)
       integer(4) :: j4=abs(-1_4)
       integer(8) :: j8=abs(-1_8)

       complex :: c=abs((-2.0,1.0))
       complex(4) :: c4=abs((1.0_4, 1.0_4))
       complex(8) :: c8=abs((1.0_8, 1.0_8))
       complex(16) :: c16=abs((1.0Q0, 1.0_16))

       end program
