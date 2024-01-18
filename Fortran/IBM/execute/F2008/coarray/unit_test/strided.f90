!*  ===================================================================
!*
!*  DATE                       : July 31, 2009
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Strided coarray access
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : functional testing of strided coarray
!*                               access. This test case was
!*                               originally from Rice Univ. Changes
!*                               have been made to enhance it.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  program Strided
  implicit none
  integer, parameter :: n1=5, n2=4, n3=3
  integer, save :: caf3(1:n1,1:n2,0:n3-1)[*]
  integer me, nprocs
  integer i, j, k, test
  integer caseid

  me = this_image()
  nprocs = num_images()
  if (nprocs == 1) then
     print *, 'the program must run on at least 2 CPUs'
     !go to 111
     error stop 111
  end if

  caseid = 1
  caf3(:,:,:) = me
  sync all
  if (me == 2) then
     caf3(:,:,:)[1] = caf3(:,:,:)
  endif
  sync all
  if (me == 1) then
     !test = 0
     do i = 1, n1
        do j = 1, n2
           do k = 0, n3-1
              if (caf3(i,j,k) /= 2) then
                 !test = 1
                 call zzrc(caseid)
              end if
           end do
        end do
     end do
     !print *,'Testing contiguous 3d section Put from image 2...'
     !if (test == 0) then
     !   print *,'   passed...'
     !   print *,caf3
     !else
     !   print *,'   failed...'
     !   print *,caf3
     !end if
  end if
  sync all


  caseid = 2
  caf3(:,:,:) = me
  sync all
  if (me == 2) then
     caf3(1:n1:4,1:n2:3,0:n3-1:2)[1] = caf3(1:n1:4,1:n2:3,0:n3-1:2)
  endif
  sync all
  if (me == 1) then
     !test = 0
     do i = 1, n1, 4
        do j = 1, n2, 3
           do k = 0, n3-1, 2
              if (caf3(i,j,k) /= 2) then
                 !test = 1
                 call zzrc(caseid)
              end if
           end do
        end do
     end do

     !print *,'Testing strided (level=3) 3d section Put from image 2...'
     !if (test == 0) then
     !   print *,'   passed...'
     !   print *,caf3
     !else
     !   print *,'   failed...'
     !   print *,caf3
     !end if
  end if
  sync all


  caseid = 3
  caf3(:,:,:) = me
  sync all
  if (me == 2) then
     caf3(1:n1,1:n2:3,0:n3-1:2)[1] = caf3(1:n1,1:n2:3,0:n3-1:2)
  endif
  sync all
  if (me == 1) then
     !test = 0
     do i = 1, n1
        do j = 1, n2, 3
           do k = 0, n3-1, 2
              if (caf3(i,j,k) /= 2) then
                 !test = 1
                 call zzrc(caseid)
              end if
           end do
        end do
     end do
     !print *,'Testing strided (level=2) 3d section Put from image 2...'
     !if (test == 0) then
     !   print *,'   passed...'
     !   print *,caf3
     !else
     !   print *,'   failed...'
     !   print *,caf3
     !end if
  end if
  sync all

  caseid = 4
  caf3(:,:,:) = me
  sync all
  if (me == 2) then
     caf3(1:n1,1:n2,0:n3-1:2)[1] = caf3(1:n1,1:n2,0:n3-1:2)
  endif
  sync all
  if (me == 1) then
     !test = 0
     do i = 1, n1
        do j = 1, n2
           do k = 0, n3-1, 2
              if (caf3(i,j,k) /= 2) then
                 !test = 1
                 call zzrc(caseid)
              end if
           end do
        end do
     end do
     !print *,'Testing strided (level=1) 3d section Put from image 2...'
     !if (test == 0) then
     !   print *,'   passed...'
     !   print *,caf3
     !else
     !   print *,'   failed...'
     !   print *,caf3
     !end if
  end if
  sync all

  caseid = 5
  caf3(:,:,:) = me
  sync all
  if (me == 2) then
     caf3(1:n1,2:n2,0:n3-1)[1] = caf3(1:n1,2:n2,0:n3-1)
  endif
  sync all
  if (me == 1) then
     !test = 0
     do i = 1, n1
        do j = 2, n2
           do k = 0, n3-1
              if (caf3(i,j,k) /= 2) then
                 !test = 1
                 call zzrc(caseid)
              end if
           end do
        end do
     end do
     !print *,'Testing strided (level=1) 3d section Put from image 2...'
     !if (test == 0) then
     !   print *,'   passed...'
     !   print *,caf3
     !else
     !   print *,'   failed...'
     !   print *,caf3
     !end if
  end if
  sync all

  caseid = 6
  caf3(:,:,:) = me
  sync all
  if (me == 2) then
     caf3(1:n1,2:n2:2,0:n3-1)[1] = caf3(1:n1,2:n2:2,0:n3-1)
  endif
  sync all
  if (me == 1) then
     !test = 0
     do i = 1, n1
        do j = 2, n2, 2
           do k = 0, n3-1
              if (caf3(i,j,k) /= 2) then
                 !test = 1
                 call zzrc(caseid)
              end if
           end do
        end do
     end do
     !print *,'Testing strided (level=2) 3d section Put from image 2...'
     !if (test == 0) then
     !   print *,'   passed...'
     !   print *,caf3
     !else
     !   print *,'   failed...'
     !   print *,caf3
     !end if
  end if
  sync all

  caseid = 7
  caf3(:,:,:) = me
  sync all
  if (me == 2) then
     caf3(1:n1,2:2,0:n3-1)[1] = caf3(1:n1,3:3,0:n3-1)
  endif
  sync all
  if (me == 1) then
     !test = 0
     do i = 1, n1
        do j = 2, 2
           do k = 0, n3-1
              if (caf3(i,j,k) /= 2) then
                 !test = 1
                 call zzrc(caseid)
              end if
           end do
        end do
     end do
     !print *,'Testing strided (level=1) 3d section Put from image 2...'
     !if (test == 0) then
     !   print *,'   passed...'
     !   print *,caf3
     !else
     !   print *,'   failed...'
     !   print *,caf3
     !end if
  end if
  sync all

  caseid = 8
  caf3(:,:,:) = me
  sync all
  if (me == 2) then
     caf3(2:2,3:3,1:1)[1] = caf3(1:1,3:3,0:0)
  endif
  sync all
  if (me == 1) then
     !test = 0
     do i = 2, 2
        do j = 3, 3
           do k = 1, 1
              if (caf3(i,j,k) /= 2) then
                 !test = 1
                 call zzrc(caseid)
              end if
           end do
        end do
     end do
     !print *,'Testing 1-element 3d section Put from image 2...'
     !if (test == 0) then
     !   print *,'   passed...'
     !   print *,caf3
     !else
     !   print *,'   failed...'
     !   print *,caf3
     !end if
  end if
  sync all

  caseid = 9
  caf3(:,:,:) = me
  sync all
  if (me == 2) then
     caf3(1:n1,1:n2:2,0:0)[1] = caf3(1:n1,1:n2:2,0:0)
  endif
  sync all
  if (me == 1) then
     !test = 0
     do i = 1, n1
        do j = 1, n2, 2
           do k = 0, 0
              if (caf3(i,j,k) /= 2) then
                 !test = 1
              end if
           end do
        end do
     end do
     !print *,'Testing strided (level=1) 3d section Put from image 2...'
     !if (test == 0) then
     !   print *,'   passed...'
     !   print *,caf3
     !else
     !   print *,'   failed...'
     !   print *,caf3
     !end if
  end if
  sync all



!111  sync all


end program STRIDED
