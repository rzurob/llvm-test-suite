      ! Error: Both unit and newunit specified
      ! NEWUNIT= should be ignored after we issue the message,
      ! so there shouldn't be a message about needing
      ! FILE= or STATUS='SCRATCH'
      integer :: i = 10
      open(newunit=i, unit=i)    ! error
      write(i, *) 'hello world'
      close(i)
      end
