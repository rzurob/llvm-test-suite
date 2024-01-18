      ! concatination involves allocatable non-coarray characters
      implicit none
      character*15, save :: c1 = '---------------'
      character(:), allocatable :: c2
      character*5, save :: cco[*]
      integer :: me, i
      me = this_image()

      cco = achar(me+70)//'1234' ! 'G', 'H', ...

      allocate(character*5::c2)

      c2 = '#$#$#'

      sync all

      if (me .eq. 1) then
         i = num_images()
      else
         i = me-1
      end if

      c1 = cco[i] // cco // c2

      print *, "1:", c1, "."

      sync all

      if (me == 1) then
         cco = '12345'
      else
         cco = '67890'
      end if

      c2 =  'abcde'

      sync all

      c1 = cco[1] // c2 // cco

      print *, "2:", c1, "."
      print *, "3:", cco[1]


      end
