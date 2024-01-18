!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : cafsum02.f
!*
!*  PROGRAMMER                 : Xing Xue
!*  DATE                       : July 31, 2009
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray access
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  DRIVER STANZA              : xlf95_r
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : functional testing of coarray
!*                               access.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

	program cafsum02
	integer, save :: coarray(10)[*], sum[*]
	integer me, i, verify_sum
        
        me = THIS_IMAGE()
        coarray(:) = me
        sum = 0
        SYNC ALL
        ! Images other than image #1 calculate the sum of
        ! 'coarray' of their neighbour (image me-1).
        if (me <> 1) then
          do i=1, 10
            sum = sum + coarray(i)[me - 1]
          end do
        end if
        SYNC ALL
        ! Image #1 calculateis the total sum from sum of other images
        if (me == 1) then
          print *, "num_images =", NUM_IMAGES()
          verify_sum = 0
          do i = 2, NUM_IMAGES()
            sum = sum + sum[i]
            verify_sum = verify_sum + (i - 1) * 10
          end do
          if (sum .ne. verify_sum) then
            error stop 10 
          end if
          print *, "The sum is:", sum
        end if
        SYNC ALL
        end 
