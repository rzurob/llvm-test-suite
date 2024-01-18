! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: iostatendmixlog001.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : iostatendmixlog001 
!*
!*  PROGRAMMER                 : Rob Wheeler
!*  DATE                       : Jan 20, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : is_iostat_eor 
!*  SECONDARY FUNCTIONS TESTED : None 
!*
!*  DRIVER STANZA              : xlf
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  DESCRIPTION                : Ensure that basic funcationailty works for instrinsic when reads and uses a format line
	PROGRAM iostatendmixlog001
	INTEGER , DIMENSION(11) :: A 
	character(4)::dword
	INTEGER :: ios,ios2
	INTEGER :: I
	OPEN(UNIT=1,FILE='fmtdat.txt',STATUS='OLD')
	DO I=1,10
		READ (UNIT=1,FMT=10,IOSTAT=ios) A(I)
		10 FORMAT(I3)
		IF (.not. is_iostat_end(ios)) THEN
			CYCLE
		ELSE
			PRINT *,' End of file detected at line ',I
			PRINT *,' Please check data file'
			EXIT
		ENDIF
	END DO
	
	DO I=1,10
		PRINT * , ' I = ',I,' A(I) = ',A(I)
	ENDDO
	open( 2, file='file1.txt', action='read' ) 
	do j=1,5
         read( 2,*,iostat=ios2 ) dword
         if (is_iostat_end(ios2)) then
         dword="eof "
         endif
         write(6,*) "ios = ", ios
         write(6,*) "four letter word = ", dword
 	enddo
	
	
	!should be at one from end of both files
	print *, "case 1 "
	if(is_iostat_end(ios) .and. is_iostat_end(ios2) .and. .true.) then
		print *, "bad" 
	else
	  print *,"good" ! f,f,t
	endif
	
	READ (UNIT=1,FMT=10,IOSTAT=ios) A(I)
	print *, "case 2 "
	if(is_iostat_end(ios) .and. is_iostat_end(ios2) .and. .true.) then
		print *, "bad" ,ios,ios2
	else
	  print *,"good" ! t,f,t
	endif
  
	read( 2,*,iostat=ios2 ) dword
	print *, "case 3 "
	if(is_iostat_end(ios) .and. is_iostat_end(ios2) .and. .true.) then
		print *, "good" ! t,t,t
	else
	  print *,"bad" ,ios,ios2
	endif
	
	END PROGRAM iostatendmixlog001