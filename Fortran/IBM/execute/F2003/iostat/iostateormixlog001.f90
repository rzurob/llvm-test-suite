! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: iostateormixlog001.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan 20, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : is_iostat_eor
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Ensure that basic funcationailty works for instrinsic when reads and uses a format line
	PROGRAM iostateormixlog001
	INTEGER , DIMENSION(11) :: A
	character(4)::dword
	INTEGER :: ios,ios2
	INTEGER :: I=1
	OPEN(UNIT=1,FILE='fmtdat.txt',STATUS='OLD')
		READ (UNIT=1,FMT=10,IOSTAT=ios,advance="no") A(I)
		10 FORMAT(I3)
		IF (is_iostat_eor(ios)) THEN
			PRINT *,' End of file detected at line ',I
			PRINT *,' Please check data file'
		ENDIF

	open( 2, file='file1.txt', action='read' )
	       read( 2,'(A4)',iostat=ios2,advance="no" ) dword
         if (is_iostat_eor(ios2)) then
         dword="eor "
         endif
         write(6,*) "ios = ", ios
         write(6,*) "four letter word = ", dword

	!should be at one from end of both files
	print *, "case 1 "
	if(is_iostat_eor(ios) .and. is_iostat_eor(ios2) .and. .true.) then
		print *, "bad"
	else
	  print *,"good" ! f,f,t
	endif

	READ (UNIT=1,FMT=10,IOSTAT=ios,advance="no") A(I)
	print *, "case 2 "
	if(is_iostat_eor(ios) .and. is_iostat_eor(ios2) .and. .true.) then
		print *, "bad"
	else
	  print *,"good" ! t,f,t
	endif

	read( 2,'(a4)',iostat=ios2 ,advance="no") dword
	print *, "case 3 "
	if(is_iostat_eor(ios) .and. is_iostat_eor(ios2) .and. .true.) then
		print *, "good" ! t,t,t
	else
	  print *,is_iostat_eor(ios)
	  print *,is_iostat_eor(ios2)
	  print *,"bad"
	endif

	END PROGRAM iostateormixlog001
