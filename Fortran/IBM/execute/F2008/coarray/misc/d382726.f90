      ! this is a small test case for using array elements as
      ! coindexes.

        integer*8, save :: coarr(10)[2,*]
        integer*8, save :: csc[2,*]
        integer*8 :: cosubs(2), res, me

        if (num_images .lt. 2) stop 1
        
        res = 0
        me = this_image()
        coarr(10) = me
        csc = me
        cosubs = this_image(coarr)

        sync all

        if (coarr(10)[cosubs(1), cosubs(2)] .ne. me) then
           error stop 1
        end if

        if (csc[cosubs(1), cosubs(2)] .ne. me) then
           error stop 2
        end if

        if (coarr(10)[abs(coarr(10)[2,1]), myfunc(coarr(10)[1,1] + res )] .ne. 2) then
           error stop 3
        end if

        if (csc[abs(coarr(10)[2,1]), myfunc(coarr(10)[1,1])] .ne. 2) then
           error stop 4
        end if

        contains

        integer*8 function myfunc(arg)
          integer*8 :: arg
          myfunc = arg
        end

        end
