      ! testing substrings
      implicit none
      character(10), save :: str(10)[*]
      character(2) s1, s2, s3
      integer :: me
      me = this_image()

      if (me == 1) then
         str(3)(4:5)[4] = 'ab'
         str(3)(6:7)[4] = 'cd'
         str(2)(4:5)[4] = 'ef'
      end if

      sync all

      s1 = str(3)(6:7)[4]
      s2 = str(3)(4:5)[4]
      s3 = str(2)(4:5)[4]
      print *, "1:", s1, s2, s3

      sync all

      str = 'abc' // achar(ichar('0')+me) // 'fghijh'

      sync all

      s1 = str(3)(4:5)[me]    ! substring 4:5 of 3rd array element of image 2
      print *, "2:", s1
      print *, "3:", str(3)

      end

