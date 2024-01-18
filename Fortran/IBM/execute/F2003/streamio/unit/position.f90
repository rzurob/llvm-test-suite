!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: position.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************

!*  ===================================================================
!*
!*  DATE                       : April 2003
!*  ORIGIN                     : AIX Compiler Development,
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
!*  DESCRIPTION                : POSITION= specifier
!*                               INQUIRE Statement
!*
!234567890123456789012345678901234567890123456789012345678901234567890
character(3) c
integer x, y

open(12, status='replace', access='stream', form='formatted')
write(12, '(A5)') '12345'
close(12, status='keep')

open(12, status='old', access='stream', position='append')
inquire(12, stream=c, pos=x, size=y)
if (c /= 'YES') error stop 1
if (x /= 7) error stop 2
if (y /= 6) error stop 3
close(12, status='delete')

end
