      ! NEWUNIT= requires either STATUS='scratch' or FILE=
      integer i
      open(newunit=i)           ! error
      write(i, *) 'hello world'
      end
