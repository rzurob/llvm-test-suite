! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: iostateofread1.f
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
!*  TEST CASE TITLE            : iostateofread1 
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
!*  DESCRIPTION                : Ensure that basic funcationailty works for instrinsic when reads a file via command line

	SUBROUTINE Readin(Name,X,Y,N,iarr)
	IMPLICIT NONE
	INTEGER , INTENT(IN) :: N
	REAL,DIMENSION(1:N),INTENT(OUT)::X,Y
	integer,dimension(1:n),intent(out)::iarr
	CHARACTER (LEN=20),INTENT(IN)::Name
	INTEGER::I,ios
	OPEN(UNIT=10,STATUS='OLD',FILE=Name)
	DO I=1,N
		READ(10,*,IOSTAT=ios)X(I),Y(I)
		iarr(i)=ios
	END DO   
	READ(10,*,iostat=ios)X(I),Y(I)
	iarr(4)=ios
	if (is_iostat_end(ios)) then
	  print *, "end of the file reached"
	endif
	CLOSE(UNIT=10) 
	END SUBROUTINE Readin
	
	PROGRAM readvals
	IMPLICIT NONE
	REAL,DIMENSION(1:100)::A,B
	INTEGER :: Nos,I
	integer, dimension(1:100)::iostatarr
	CHARACTER(LEN=20)::Filename
	
	INTERFACE
	SUBROUTINE Readin(Name,X,Y,N,iarr)
		IMPLICIT NONE
		INTEGER , INTENT(IN) :: N
		REAL,DIMENSION(1:N),INTENT(OUT)::X,Y
		integer,dimension(1:n),intent(out)::iarr
		CHARACTER (LEN=20),INTENT(IN)::Name
	END SUBROUTINE Readin
	END INTERFACE
	
	PRINT *,' Type in the name of the data file'
	READ '(A)' , Filename
	PRINT *,' Input the number of items in the file'
	READ * , Nos
	CALL Readin(Filename,A,B,Nos,iostatarr)
	PRINT * , ' Data read in was'
	DO I=1,Nos
		PRINT *,' ',A(I),' ',B(I),iostatarr(i)
	ENDDO
	print *,iostatarr(4)
	END PROGRAM readvals