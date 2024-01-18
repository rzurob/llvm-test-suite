! *********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         INBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : iostatendbranch001.f
!
!* PROGRAMMER                   : Rob Wheeler
!* DATE                         : Feb 1, 2003

!234567890123456789012345678901234567890123456789012345678901234567890
program iostatendbranch
  character(4) c
  integer(4) :: I = 5, J = 2, K, ios
  open( 1, file='file1.txt', action='read' ) 
	do while( J .eq. 2 )
         read( 1,'(A4)',iostat=ios ,advance='no', EOR=200,ERR=100) c
         if (is_iostat_eor(ios)) then
         c="eor "
         endif
         write(6,*) "ios = ", ios
         write(6,*) "four letter word = ", c
	enddo
  
100   print *, 'Error encountered'
200   PRINT *, 'Reached the end of the unit.'
201   Print *, 'Branch'
      print *, 'is_iostat_end: ',is_iostat_end(ios)
      print *, 'is_iostat_eor: ',is_iostat_eor(ios)
end program iostatendbranch

  
