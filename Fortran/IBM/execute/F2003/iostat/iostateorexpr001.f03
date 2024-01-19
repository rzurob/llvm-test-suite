!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan 20, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : is_iostat_eor
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Ensure that basic funcationailty works for instrinsic when reads and uses a format line
	PROGRAM iostateorexpr001
	INTEGER , DIMENSION(11) :: A
	INTEGER :: ios
	INTEGER :: I
	OPEN(UNIT=1,FILE='fmtdat.txt',STATUS='OLD')
	DO I=1,9
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

	DO I=1,9
		PRINT * , ' I = ',I,' A(I) = ',A(I)
	ENDDO
	i=10
	READ (UNIT=1,FMT=10,IOSTAT=ios,advance="no") A(I)
	IF (is_iostat_eor(ios)) THEN
		PRINT *,' End of record detected at line ',I
	endif
	i=11
	READ (UNIT=1,FMT=10,IOSTAT=ios,advance="no") A(I)
	IF (is_iostat_eor(ios)) THEN
		PRINT *,' End of record detected at line ',I
	endif

	write(6,*) "expr = ios(which should be -1)+2+(2*4)-8-2: ",is_iostat_eor(ios+2+(2*4)-8-2)
     write(6,*) "expr = ios(which should be -1)+2*max(3,4)-8: ",is_iostat_eor(ios+2*max(3,4)-8)
     write(6,*) "expr = ios(which should be -1)+2*max(3,4)-8**3: ",is_iostat_eor(ios+2*max(3,4)-8**3)
	END PROGRAM iostateorexpr001