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
	PROGRAM iostatfmt
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
	IF (is_iostat_eor(i=ios)) THEN
	  PRINT *,' small i works '
		PRINT *,' End of record detected at line ',I
	endif
	IF (is_iostat_eor(I=ios)) THEN
	  PRINT *,' big I works '
		PRINT *,' End of record detected at line ',I
	endif
	END PROGRAM iostatfmt