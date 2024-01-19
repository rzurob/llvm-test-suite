!**********************************************************************

!*  ===================================================================
!*
!*  DATE                       : April 2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : Backspace on unformatted stream
!*
!234567890123456789012345678901234567890123456789012345678901234567890
       integer iostat, i, z

       open(11, access='sequential', status='replace', form='formatted')
       write(11, fmt='(A3,/,A3)') 'abc', 'def'
       close(11, status='keep')

       open(11, access='stream', status='old', form='unformatted', position='append')
       backspace(11, iostat=iostat)
       if (iostat /= 194) error stop 1

       backspace(11)
       close(11, status='delete')
       end
