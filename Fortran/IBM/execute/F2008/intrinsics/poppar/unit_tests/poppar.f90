       ! Test F2008 changes to poppar
       !   1. poppar can appear in constant expressions.
       !   2. F2008 allows only integer arguments to poppar
       implicit none

       integer(1), parameter :: i1 = 1
       integer(2), parameter :: i2 = 2
       integer(4), parameter :: i4 = 3
       integer(8), parameter :: i8 = 4

       byte, parameter :: b = 5

       logical(1), parameter :: l1 = .true.
       logical(2), parameter :: l2 = .false.
       logical(4), parameter :: l4 = .true.
       logical(8), parameter :: l8 = .false.

       real(4), parameter :: r4 = 1.0e0
       real(8), parameter :: r8 = z'7ff0000000000000'

       integer, parameter :: res1 = poppar(i1)
       integer, parameter :: res2 = poppar(i2)
       integer, parameter :: res3 = poppar(i4)
       integer, parameter :: res4 = poppar(i8)

       integer, parameter :: res5 = poppar(b)

       integer, parameter :: res6 = poppar(l1)
       integer, parameter :: res7 = poppar(l2)
       integer, parameter :: res8 = poppar(l4)
       integer, parameter :: res9 = poppar(l8)

       integer, parameter :: res10 = poppar(r4)
       integer, parameter :: res11 = poppar(r8)

       if (res1 /= 1) error stop 1
       if (res2 /= 1) error stop 2
       if (res3 /= 0) error stop 3
       if (res4 /= 1) error stop 4

       if (res5 /= 0) error stop 5

       if (res6 /= 1) error stop 6
       if (res7 /= 0) error stop 7
       if (res8 /= 1) error stop 8
       if (res9 /= 0) error stop 9

       if (res10 /= 1) error stop 10
       if (res11 /= 1) error stop 11

       end 
