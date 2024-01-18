      ! this is a small test case for the copy-in/copy-out 
      ! coindexed objects.

      integer, save :: coarr(-1:8)[*]

      coarr = -1

      sync all

      if (this_image() == 1) then
         call foo(coarr(:)[10], 1)
         call foo(coarr[5], 2)
      end if      

      sync all

      if (this_image() == 10) then
         if (any(coarr .ne. [1,2,3,4,5,6,7,8,9,10])) error stop 2
      else if (this_image() == 5 ) then
         if (any(coarr .ne. [11,12,13,14,15,16,17,18,19,20])) error stop 3
      else
         if (any(coarr .ne. [-1,-1,-1,-1,-1,-1,-1,-1,-1,-1])) error stop 4
      end if

      sync all

      contains
      subroutine foo(arr, code)
         integer :: arr(10)
         integer :: code

         if (any(coarr .ne. [-1,-1,-1,-1,-1,-1,-1,-1,-1,-1])) error stop 1

         if (code == 1) then
            arr = [1,2,3,4,5,6,7,8,9,10]
         else
            arr = [11,12,13,14,15,16,17,18,19,20]
         end if

      end subroutine
      end
