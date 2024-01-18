       ! Test F2008 changes to popcnt
       !   1. popcnt can appear in constant expressions.
       !   2. F2008 allows only integer arguments to popcnt
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

       integer, parameter :: res1 = popcnt(i1)
       integer, parameter :: res2 = popcnt(i2)
       integer, parameter :: res3 = popcnt(i4)
       integer, parameter :: res4 = popcnt(i8)

       integer, parameter :: res5 = popcnt(b)

       integer, parameter :: res6 = popcnt(l1)
       integer, parameter :: res7 = popcnt(l2)
       integer, parameter :: res8 = popcnt(l4)
       integer, parameter :: res9 = popcnt(l8)

       integer, parameter :: res10 = popcnt(r4)
       integer, parameter :: res11 = popcnt(r8)

       if (res1 /= 1) error stop 1
       if (res2 /= 1) error stop 2
       if (res3 /= 2) error stop 3
       if (res4 /= 1) error stop 4

       if (res5 /= 2) error stop 5

       if (res6 /= 1) error stop 6
       if (res7 /= 0) error stop 7
       if (res8 /= 1) error stop 8
       if (res9 /= 0) error stop 9

       if (res10 /= 7) error stop 10
       if (res11 /= 11) error stop 11

       end
