! *********************************************************************
!* ===================================================================
!*
!
!* DATE                         : Feb 1, 2003

!234567890123456789012345678901234567890123456789012345678901234567890
program iostatendbranch
  character(4) c
  integer(4) :: I = 5, J = 2, K, ios

  open(1, file='mystream', access='stream', form='formatted', &
       action='readwrite')

  ! Attempt to do non-advancing character input starting
  ! at file position 5.


  read(1, FMT='(A4)', ADVANCE='NO', EOR=100,END=101 , POS=I, iostat=ios) c
  print *,"it doesn't"


  ! Overwrite positions 2-5 (inclusive) with the value
  ! of the 4-byte integer variable I.
  write(1, FMT='(I4)', ADVANCE='NO', POS=J) I
  inquire(1, pos=K)
  print *, K
  close(1)

100   PRINT *, 'Reached the end of the unit.'
101   Print *, 'Branch'
      print *, 'is_iostat_end: ',is_iostat_end(ios)
end program iostatendbranch

