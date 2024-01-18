! Call to minloc an exec statement with back = .true. and
! 1. rank 1 integer*8 array 
! 2. rank 1 integer*8 array and mask
! 3. rank 2 integer*8 array
! 4. rank 2 integer*8 array and mask
! 5. rank 2 integer*8 array and dim
! 6. rank 2 integer*8 array, dim and mask
! 7. rank 3 integer*8 array
! 8. rank 3 integer*8 array and mask
! 9. rank 3 integer*8 array and dim
! 10. rank 3 integer*8 array, dim and mask

implicit none

integer :: j1(1), j2(1)
integer :: j3(2), j4(2), j5(2), j6(2)
integer :: j7(3), j8(3)
integer :: j9(2,2), j10(2,2)

j1 = minloc((/3_8, 1_8, 1_8, 2_8/), back = .true.)

j2 = minloc((/3_8, 2_8, 1_8, 2_8/), (/.true., .true., .false., .true./), back = .true.)

j3 = minloc(reshape((/3_8, 1_8, 1_8, 2_8/), (/2,2/)), back = .true.)

j4 = minloc(reshape((/3_8, 2_8, 1_8, 2_8/), (/2,2/)), &
            reshape((/.true., .true., .false., .true./), (/2,2/)), back = .true.)

j5 = minloc(reshape((/3_8, 1_8, 1_8, 2_8/), (/2,2/)), 2, back = .true.)

j6 = minloc(reshape((/1_8, 3_8, 2_8, 3_8/), (/2,2/)), 2, &
            reshape((/.true., .true., .false., .true./), (/2,2/)), back = .true.)

j7 = minloc(reshape((/1_8, 3_8, 4_8, 2_8, 1_8, 3_8, 4_8, 2_8/), (/2,2,2/)), back = .true.)

j8 = minloc(reshape((/2_8, 3_8, 1_8, 3_8, 2_8, 3_8, 4_8, 3_8/), (/2,2,2/)), &
            reshape((/.true., .true., .false., .true., &
                      .true., .true., .true., .true./), (/2,2,2/)), back = .true.)

j9 = minloc(reshape((/2_8, 4_8, 1_8, 4_8, 2_8, 4_8, 4_8, 3_8/), (/2,2,2/)), 2, back =.true.)

j10 = minloc(reshape((/2_8, 4_8, 1_8, 4_8, 2_8, 4_8, 4_8, 3_8/), (/2,2,2/)), 2, &
             reshape((/.true., .true., .false., .true., &
                       .true., .true., .true., .true./), (/2,2,2/)), back =.true.)
                          
if (j1(1) .ne. 3) ERROR STOP 1
if (j2(1) .ne. 4) ERROR STOP 2
if (j3(1) .ne. 1 .or. j3(2) .ne. 2) ERROR STOP 3
if (j4(1) .ne. 2 .or. j4(2) .ne. 2) ERROR STOP 4
if (j5(1) .ne. 2 .or. j5(2) .ne. 1) ERROR STOP 5
if (j6(1) .ne. 1 .or. j6(2) .ne. 2) ERROR STOP 6
if (j7(1) .ne. 1 .or. j7(2) .ne. 1 .or. j7(3) .ne. 2) ERROR STOP 7
if (j8(1) .ne. 1 .or. j8(2) .ne. 1 .or. j8(3) .ne. 2) ERROR STOP 8
if (j9(1,1) .ne. 2 .or. j9(1,2) .ne. 1 .or. j9(2,1) .ne. 2 .or. j9(2,2) .ne. 2 ) ERROR STOP 9
if (j10(1,1) .ne. 1 .or. j10(1,2) .ne. 1 .or. j10(2,1) .ne. 2 .or. j10(2,2) .ne. 2 ) ERROR STOP 10

end
