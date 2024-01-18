! Call to maxloc in initialization expression with rank 2 character array and
! 1. no BACK argument
! 2. BACK = .true.
! 3. BACK = .false.
! 4. MASK is present and BACK = .true.
! 5. MASK is present and BACK = .false.
! 6. MASK and KIND are present and BACK = .true.
! 7. MASK and KIND are present and BACK = .false.
! 8. KIND is present and BACK = .true.
! 9. KIND is present and BACK = .false.
! 10. DIM is present and BACK = .true.
! 11. DIM is present and BACK = .false.
! 12. DIM and MASK are present and BACK = .true.
! 13. DIM and MASK are present and BACK = .false.
! 14. DIM, MASK and KIND are present and BACK = .true.
! 15. DIM, MASK and KIND are present and BACK = .false.

implicit none

integer :: j1(2) = maxloc(reshape((/'a', 'c', 'c', 'b'/), (/2,2/)))

integer :: j2(2) = maxloc(reshape((/'a', 'c', 'c', 'b'/), (/2,2/)), back = .true.)

integer :: j3(2) = maxloc(reshape((/'aa', 'cc', 'cc', 'bb'/), (/2,2/)), back = .false.)

integer :: j4(2) = maxloc(reshape((/'a', 'b', 'c', 'b'/), (/2,2/)), &
                          reshape((/.true., .true., .false., .true./), (/2,2/)), & 
                          back = .true.)

integer :: j5(2) = maxloc(reshape((/'a', 'b', 'c', 'b'/), (/2,2/)), &
                          mask = reshape((/.true., .true., .false., .true./), (/2,2/)), &
                          back = .false.)

integer :: j6(2) = maxloc(reshape((/'aaaa', 'bbbb', 'cccc', 'bbbb'/), (/2,2/)), &
                          reshape((/.true., .true., .false., .true./), (/2,2/)), &
                          4, .true.)

integer :: j7(2) = maxloc(reshape((/'a', 'b', 'c', 'b'/), (/2,2/)), &
                          reshape((/.true., .true., .false., .true./), (/2,2/)), &
                          kind = 4, back = .false.)

integer :: j8(2) = maxloc(reshape((/'a', 'c', 'b', 'c'/), (/2,2/)), kind = 4, back = .true.)

integer :: j9(2) = maxloc(reshape((/'a', 'c', 'b', 'c'/), (/2,2/)), kind = 4, back = .false.)

integer :: j10(2) = maxloc(reshape((/'a', 'c', 'b', 'c'/), (/2,2/)), 2, back = .true.)

integer :: j11(2) = maxloc(reshape((/'a', 'c', 'b', 'c'/), (/2,2/)), 2, back = .false.) 

integer :: j12(2) = maxloc(reshape((/'a', 'c', 'b', 'c'/), (/2,2/)), 2, &
                           reshape((/.true., .true., .false., .true./), (/2,2/)), &
                           back =.true.)

integer :: j13(2) = maxloc(reshape((/'a', 'c', 'b', 'c'/), (/2,2/)), 1, &
                           reshape((/.true., .true., .false., .true./), (/2,2/)), &
                           back =.false.)  
                         
integer :: j14(2) = maxloc(reshape((/'a', 'c', 'c', 'b'/), (/2,2/)), 1, &
                           .true., 4, .true.)

integer :: j15(2) = maxloc(reshape((/'a', 'c', 'b', 'c'/), (/2,2/)), 1, &
                           reshape((/.true., .true., .false., .true./), (/2,2/)), &
                           4, .false.)

if (j1(1) .ne. 2 .or. j1(2) .ne. 1) ERROR STOP 1
if (j2(1) .ne. 1 .or. j2(2) .ne. 2) ERROR STOP 2
if (j3(1) .ne. 2 .or. j3(2) .ne. 1) ERROR STOP 3
if (j4(1) .ne. 2 .or. j4(2) .ne. 2) ERROR STOP 4
if (j5(1) .ne. 2 .or. j5(2) .ne. 1) ERROR STOP 5
if (j6(1) .ne. 2 .or. j6(2) .ne. 2) ERROR STOP 6
if (j7(1) .ne. 2 .or. j7(2) .ne. 1) ERROR STOP 7
if (j8(1) .ne. 2 .or. j8(2) .ne. 2) ERROR STOP 8
if (j9(1) .ne. 2 .or. j9(2) .ne. 1) ERROR STOP 9
if (j10(1) .ne. 2 .or. j10(2) .ne. 2) ERROR STOP 10
if (j11(1) .ne. 2 .or. j11(2) .ne. 1) ERROR STOP 11
if (j12(1) .ne. 1 .or. j12(2) .ne. 2) ERROR STOP 12
if (j13(1) .ne. 2 .or. j13(2) .ne. 2) ERROR STOP 13
if (j14(1) .ne. 2 .or. j14(2) .ne. 1) ERROR STOP 14
if (j15(1) .ne. 2 .or. j15(2) .ne. 2) ERROR STOP 15

end
