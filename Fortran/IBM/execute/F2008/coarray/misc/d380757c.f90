      implicit none
      character(6) :: c1
      character(2), save :: c2[*], c3
 
      character*10, save :: cc1 = '----------'
      character*5, save :: cco[*]
 
      integer :: me, i
      me = this_image()
      c3 = '!;'
      c2 = achar(me+ichar('a')-1) // achar(me+ichar('b')-1)
      cco = achar(me+70)//'1234' ! 'G', 'H', ...
 
      sync all

      c1 = c2 // c2[1] // c3

      print *, "1:", c1, "."

      c1 = c2[2]

      print *, "2:", c1, "."

      c1 = c2[3] // c2

      print *, "3:", c1, "."

      sync all

      c2[1] = c1 ! truncate

      print *, "4:", c2[1], "."

      sync all

      if (me .eq. 1) then
         i = num_images()
      else
         i = me-1
      end if

      cc1 = cco[i] ! pad

      print *, "5:", cc1, "."



      end

