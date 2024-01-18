      ! NEWUNIT=: open a file using newunit= and use it.
      ! uses open, read, write, backspace, rewind, flush, endfile
      implicit none
      character(*), parameter :: filename = 'myfile'
      character(11) greeting
      integer i, j, iostat
      integer, parameter :: EOF_IOSTAT = -1

      open(newunit=i, file=filename, delim='quote', status='replace')
      write(i, *) i               ! record 1
      write(i, *) 'hello'         ! record 2
      flush(i)
      backspace(i)
      write(i, *) 'hello world'   ! record 2
      write(i, *) 'newunit'       ! record 3

      rewind(i)

      read(i, *) j
      if (j /= i) then
        print *, j
        stop 1
      endif

      read(i, *) greeting
      if (greeting /= 'hello world') then
        print *, greeting
        stop 2
      endif

      endfile(i)
      close(i, status='keep')

      ! verify
      j = 0
      open(10, file=filename, status='old')

      read(10, *) j
      if (j /= i) then
        print *, j
        stop 3
      endif

      read(10, *) greeting
      if (greeting /= 'hello world') then
        print *, greeting
        stop 4
      endif

      read(10, *, iostat=iostat) greeting
      if (iostat /= EOF_IOSTAT) then
        print *, iostat
        stop 5
      endif

      close(10, status='delete')
      end
