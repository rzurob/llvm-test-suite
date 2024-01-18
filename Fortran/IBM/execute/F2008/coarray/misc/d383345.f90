      real*8, save :: x(2)[*]

      if (num_images() .ne. 4) then
         stop 1
      end if
            
      x = [1,2]

      sync all

      print *, "s1 =", sum(x)
      print *, "s2 =", sum(x(:)[4])
      print *, "s3 =", sum(x[4] + 2)
      print *, "s4 =", sum(x + 1)

      sync all

      end
