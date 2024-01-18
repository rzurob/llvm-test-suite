!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : d321297
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2006-10-06
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : INQUIRE
!*
!*  SECONDARY FUNCTIONS TESTED : Defect #321297: "F2003: ICE: INQUIRE with many specifiers"
!*
!*  REFERENCE                  : defect 321297
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : none
!*
!*  KEYWORD(S)                 : inquire
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  From defect 321297: Verify that INQUIRE can be invoked with any number of parameters.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program d321297

  implicit none

  character(20) ACCESSvar, ACTIONvar, ASYNCHRONOUSvar, BLANKvar, DECIMALvar, &
       DELIMvar, DIRECTvar, ENCODINGvar, FORMvar, FORMATTEDvar, NAMEvar, &
       PADvar, POSITIONvar, READvar, READWRITEvar, ROUNDvar, SEQUENTIALvar, &
       SIGNvar, STREAMvar, UNFORMATTEDvar, WRITEvar, IOMSGvar
  logical EXISTvar, NAMEDvar, OPENEDvar, PENDINGvar
  integer IOSTATvar, NEXTRECvar, NUMBERvar, POSvar, RECLvar, SIZEvar

  call reinit

  inquire (UNIT=5, &
           ACCESS=ACCESSvar, &
	   ACTION=ACTIONvar, &
	   ASYNCHRONOUS=ASYNCHRONOUSvar, &
	   BLANK=BLANKvar, &
	   DECIMAL=DECIMALvar, &
	   DELIM=DELIMvar, &
	   DIRECT=DIRECTvar, &
           EXIST=EXISTVar, &
	   FORM=FORMvar, &
	   FORMATTED=FORMATTEDvar, &
	   NAME=NAMEvar, &
	   NAMED=NAMEDvar, &
	   NEXTREC=NEXTRECvar, &
	   NUMBER=NUMBERvar, &
	   OPENED=OPENEDvar, &
	   PAD=PADvar, &
	   PENDING=PENDINGvar, &
	   POS=POSvar, &
	   POSITION=POSITIONvar, &
	   READ=READvar, &
	   READWRITE=READWRITEvar, &
	   RECL=RECLvar, &
	   ROUND=ROUNDvar, &
	   SEQUENTIAL=SEQUENTIALvar, &
	   SIGN=SIGNvar, &
	   SIZE=SIZEvar, &
	   STREAM=STREAMvar, &
	   UNFORMATTED=UNFORMATTEDvar, &
	   WRITE=WRITEvar, &
           ERR=100, IOSTAT=IOSTATvar, IOMSG=IOMSGvar)

  goto 200

100 continue
  print *, "Error on unit 5"
200 continue
  print *, 'Unit 5: ', IOSTATvar, ', ', trim(IOMSGvar)
  print *, '; ACCESS: ', trim(ACCESSvar)
  print *, '  ACTION: ', trim(ACTIONvar)
  print *, '  ASYNCHRONOUS: ', trim(ASYNCHRONOUSvar)
  print *, '  BLANK: ', trim(BLANKvar)
  print *, '  DECIMAL: ', trim(DECIMALvar)
  print *, '  DELIM: ', trim(DELIMvar)
  print *, '  DIRECT: ', trim(DIRECTvar)
  print *, '  EXIST: ', EXISTvar
  print *, '  FORM: ', trim(FORMvar)
  print *, '  FORMATTED: ', trim(FORMATTEDvar)
  print *, '  NAME: ', trim(NAMEvar)
  print *, '  NAMED: ', NAMEDvar
  print *, '  NEXTREC: ', NEXTRECvar
  print *, '  NUMBER: ', NUMBERvar
  print *, '  OPENED: ', OPENEDvar
  print *, '  PAD: ', trim(PADvar)
  print *, '  PENDING: ', PENDINGvar
  print *, '  POS: ', POSvar
  print *, '  POSITION: ', trim(POSITIONvar)
  print *, '  READ: ', trim(READvar)
  print *, '  READWRITE: ', trim(READWRITEvar)
  print *, '  RECL: ', RECLvar
  print *, '  ROUND: ', trim(ROUNDvar)
  print *, '  SEQUENTIAL: ', trim(SEQUENTIALvar)
  print *, '  SIGN: ', trim(SIGNvar)
  print *, '  SIZE: ', SIZEvar
  print *, '  STREAM: ', trim(STREAMvar)
  print *, '  UNFORMATTED: ', trim(UNFORMATTEDvar)
  print *, '  WRITE: ', trim(WRITEvar)

  call reinit

  inquire (FILE='nosuchfile.fil', &
           ACCESS=ACCESSvar, &
	   ACTION=ACTIONvar, &
	   ASYNCHRONOUS=ASYNCHRONOUSvar, &
	   BLANK=BLANKvar, &
	   DECIMAL=DECIMALvar, &
	   DELIM=DELIMvar, &
	   DIRECT=DIRECTvar, &
           EXIST=EXISTVar, &
	   FORM=FORMvar, &
	   FORMATTED=FORMATTEDvar, &
	   NAME=NAMEvar, &
	   NAMED=NAMEDvar, &
	   NEXTREC=NEXTRECvar, &
	   NUMBER=NUMBERvar, &
	   OPENED=OPENEDvar, &
	   PAD=PADvar, &
	   PENDING=PENDINGvar, &
	   POS=POSvar, &
	   POSITION=POSITIONvar, &
	   READ=READvar, &
	   READWRITE=READWRITEvar, &
	   RECL=RECLvar, &
	   ROUND=ROUNDvar, &
	   SEQUENTIAL=SEQUENTIALvar, &
	   SIGN=SIGNvar, &
	   SIZE=SIZEvar, &
	   STREAM=STREAMvar, &
	   UNFORMATTED=UNFORMATTEDvar, &
	   WRITE=WRITEvar, &
           ERR=300, IOSTAT=IOSTATvar, IOMSG=IOMSGvar)


  goto 400

300 continue
  print *, "Error on file"
400 continue
  print *, 'File: ', IOSTATvar, ', ', trim(IOMSGvar)
  print *, '; ACCESS: ', trim(ACCESSvar)
  print *, '  ACTION: ', trim(ACTIONvar)
  print *, '  ASYNCHRONOUS: ', trim(ASYNCHRONOUSvar)
  print *, '  BLANK: ', trim(BLANKvar)
  print *, '  DECIMAL: ', trim(DECIMALvar)
  print *, '  DELIM: ', trim(DELIMvar)
  print *, '  DIRECT: ', trim(DIRECTvar)
  print *, '  EXIST: ', EXISTvar
  print *, '  FORM: ', trim(FORMvar)
  print *, '  FORMATTED: ', trim(FORMATTEDvar)
  print *, '  NAME: ', trim(NAMEvar)
  print *, '  NAMED: ', NAMEDvar
  print *, '  NEXTREC: ', NEXTRECvar
  print *, '  NUMBER: ', NUMBERvar
  print *, '  OPENED: ', OPENEDvar
  print *, '  PAD: ', trim(PADvar)
  print *, '  PENDING: ', PENDINGvar
  print *, '  POS: ', POSvar
  print *, '  POSITION: ', trim(POSITIONvar)
  print *, '  READ: ', trim(READvar)
  print *, '  READWRITE: ', trim(READWRITEvar)
  print *, '  RECL: ', RECLvar
  print *, '  ROUND: ', trim(ROUNDvar)
  print *, '  SEQUENTIAL: ', trim(SEQUENTIALvar)
  print *, '  SIGN: ', trim(SIGNvar)
  print *, '  SIZE: ', SIZEvar
  print *, '  STREAM: ', trim(STREAMvar)
  print *, '  UNFORMATTED: ', trim(UNFORMATTEDvar)
  print *, '  WRITE: ', trim(WRITEvar)

contains

  subroutine reinit
    IOMSGvar = ' ';  NAMEvar = ' '
    ACCESSvar = ' '; ACTIONvar = ' '; ASYNCHRONOUSvar = ' '; BLANKvar = ' '
    DECIMALvar = ' '; DELIMvar = ' '; DIRECTvar = ' '; ENCODINGvar = ' '
    FORMvar = ' '; FORMATTEDvar = ' '; NAMEvar = ' '; PADvar = ' '; POSITIONvar = ' '
    READvar = ' '; READWRITEvar = ' '; ROUNDvar = ' '; SEQUENTIALvar = ' '
    SIGNvar = ' '; STREAMvar = ' '; UNFORMATTEDvar = ' '; WRITEvar = ' '

    NEXTRECvar = -1; NUMBERvar = -1; POSvar = -1; RECLvar =-1; SIZEvar = -1
    EXISTVar = .false.
  end subroutine reinit

end program d321297
