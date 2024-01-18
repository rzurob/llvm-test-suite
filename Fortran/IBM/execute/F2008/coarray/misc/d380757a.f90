      implicit none
      character, save :: c1, c2, c3, c4, cco[*], cc1[*], cc2[*]
      integer :: me
      me = this_image()
      cco = achar(me+70)
      sync all

      c1 = cco
      print *, "1:", c1

      cco = c1
      print *, "2:", cco

      c1 = cco[1]
      c2 = cco[2]
      c3 = cco[3]
      c4 = cco[4]

      print *, "3:", c1, c2, c3, c4

      sync all

      cco[1] = c2
      print *, "4:", cco[1]

      sync all

      cco[1] = cco[3]
      print *, "5:", cco[1]

      cc1 = achar(me+ichar('a'))
      cc2 = achar(me+ichar('s'))
      c1 = '!'
      sync all

      print *,  "6:", cc1 // cc2[me] // c1

      end

