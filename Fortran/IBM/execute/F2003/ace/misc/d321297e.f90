!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2006-10-06
!*
!*  PRIMARY FUNCTIONS TESTED   : INQUIRE
!*
!*  SECONDARY FUNCTIONS TESTED : Defect #321297: "F2003: ICE: INQUIRE with many specifiers"
!*
!*  REFERENCE                  : defect 321297
!*
!*  REQUIRED COMPILER OPTIONS  : none
!*
!*  KEYWORD(S)                 : inquire
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  From defect 321297: Verify that INQUIRE can be invoked with all kinds of specifiers.
!*  (ENCODING was not permitted as a specifier at the time that defect 321297
!*  was opened on 2006-06-09.)
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program d321297e

  implicit none

  character(20) ACCESSvar, ACTIONvar, ASYNCHRONOUSvar, BLANKvar, DECIMALvar, &
       DELIMvar, DIRECTvar, ENCODINGvar, FORMvar, FORMATTEDvar, NAMEvar, &
       PADvar, POSITIONvar, READvar, READWRITEvar, ROUNDvar, SEQUENTIALvar, &
       SIGNvar, STREAMvar, UNFORMATTEDvar, WRITEvar, IOMSGvar
  logical EXISTvar, NAMEDvar, OPENEDvar, PENDINGvar
  integer IOSTATvar, NEXTRECvar, NUMBERvar, POSvar, RECLvar, SIZEvar


  IOMSGvar = ' '
  NAMEvar = ' '

  inquire (UNIT=5, ACCESS=ACCESSvar, IOSTAT=IOSTATvar, IOMSG=IOMSGvar)
  print *, IOSTATvar, ': ', trim(IOMSGvar), '; ACCESS: ', trim(ACCESSvar)

  inquire (UNIT=5, ACTION=ACTIONvar, IOSTAT=IOSTATvar, IOMSG=IOMSGvar)
  print *, IOSTATvar, ': ', trim(IOMSGvar), '; ACTION: ', trim(ACTIONvar)

  inquire (UNIT=5, ASYNCHRONOUS=ASYNCHRONOUSvar, IOSTAT=IOSTATvar, IOMSG=IOMSGvar)
  print *, IOSTATvar, ': ', trim(IOMSGvar), '; ASYNCHRONOUS: ', trim(ASYNCHRONOUSvar)

  inquire (UNIT=5, BLANK=BLANKvar, IOSTAT=IOSTATvar, IOMSG=IOMSGvar)
  print *, IOSTATvar, ': ', trim(IOMSGvar), '; BLANK: ', trim(BLANKvar)

  inquire (UNIT=5, DECIMAL=DECIMALvar, IOSTAT=IOSTATvar, IOMSG=IOMSGvar)
  print *, IOSTATvar, ': ', trim(IOMSGvar), '; DECIMAL: ', trim(DECIMALvar)

  inquire (UNIT=5, DELIM=DELIMvar, IOSTAT=IOSTATvar, IOMSG=IOMSGvar)
  print *, IOSTATvar, ': ', trim(IOMSGvar), '; DELIM: ', trim(DELIMvar)

  inquire (UNIT=5, DIRECT=DIRECTvar, IOSTAT=IOSTATvar, IOMSG=IOMSGvar)
  print *, IOSTATvar, ': ', trim(IOMSGvar), '; DIRECT: ', trim(DIRECTvar)

  inquire (UNIT=5, ENCODING=ENCODINGvar, IOSTAT=IOSTATvar, IOMSG=IOMSGvar)
  print *, IOSTATvar, ': ', trim(IOMSGvar), '; ENCODING: ', trim(ENCODINGvar)

  inquire (UNIT=5, FORM=FORMvar, IOSTAT=IOSTATvar, IOMSG=IOMSGvar)
  print *, IOSTATvar, ': ', trim(IOMSGvar), '; FORM: ', trim(FORMvar)

  inquire (UNIT=5, FORMATTED=FORMATTEDvar, IOSTAT=IOSTATvar, IOMSG=IOMSGvar)
  print *, IOSTATvar, ': ', trim(IOMSGvar), '; FORMATTED: ', trim(FORMATTEDvar)

  inquire (UNIT=5, NAME=NAMEvar, IOSTAT=IOSTATvar, IOMSG=IOMSGvar)
  print *, IOSTATvar, ': ', trim(IOMSGvar), '; NAME: ', trim(NAMEvar)

  inquire (UNIT=5, NAMED=NAMEDvar, IOSTAT=IOSTATvar, IOMSG=IOMSGvar)
  print *, IOSTATvar, ': ', trim(IOMSGvar), '; NAMED: ', NAMEDvar

  inquire (UNIT=5, NEXTREC=NEXTRECvar, IOSTAT=IOSTATvar, IOMSG=IOMSGvar)
  print *, IOSTATvar, ': ', trim(IOMSGvar), '; NEXTREC: ', NEXTRECvar

  inquire (UNIT=5, NUMBER=NUMBERvar, IOSTAT=IOSTATvar, IOMSG=IOMSGvar)
  print *, IOSTATvar, ': ', trim(IOMSGvar), '; NUMBER: ', NUMBERvar

  inquire (UNIT=5, OPENED=OPENEDvar, IOSTAT=IOSTATvar, IOMSG=IOMSGvar)
  print *, IOSTATvar, ': ', trim(IOMSGvar), '; OPENED: ', OPENEDvar

  inquire (UNIT=5, PAD=PADvar, IOSTAT=IOSTATvar, IOMSG=IOMSGvar)
  print *, IOSTATvar, ': ', trim(IOMSGvar), '; PAD: ', trim(PADvar)

  inquire (UNIT=5, PENDING=PENDINGvar, IOSTAT=IOSTATvar, IOMSG=IOMSGvar)
  print *, IOSTATvar, ': ', trim(IOMSGvar), '; PENDING: ', PENDINGvar

  inquire (UNIT=5, POS=POSvar, IOSTAT=IOSTATvar, IOMSG=IOMSGvar)
  print *, IOSTATvar, ': ', trim(IOMSGvar), '; POS: ', POSvar

  inquire (UNIT=5, POSITION=POSITIONvar, IOSTAT=IOSTATvar, IOMSG=IOMSGvar)
  print *, IOSTATvar, ': ', trim(IOMSGvar), '; POSITION: ', trim(POSITIONvar)

  inquire (UNIT=5, READ=READvar, IOSTAT=IOSTATvar, IOMSG=IOMSGvar)
  print *, IOSTATvar, ': ', trim(IOMSGvar), '; READ: ', trim(READvar)

  inquire (UNIT=5, READWRITE=READWRITEvar, IOSTAT=IOSTATvar, IOMSG=IOMSGvar)
  print *, IOSTATvar, ': ', trim(IOMSGvar), '; READWRITE: ', trim(READWRITEvar)

  inquire (UNIT=5, RECL=RECLvar, IOSTAT=IOSTATvar, IOMSG=IOMSGvar)
  print *, IOSTATvar, ': ', trim(IOMSGvar), '; RECL: ', RECLvar

  inquire (UNIT=5, ROUND=ROUNDvar, IOSTAT=IOSTATvar, IOMSG=IOMSGvar)
  print *, IOSTATvar, ': ', trim(IOMSGvar), '; ROUND: ', trim(ROUNDvar)

  inquire (UNIT=5, SEQUENTIAL=SEQUENTIALvar, IOSTAT=IOSTATvar, IOMSG=IOMSGvar)
  print *, IOSTATvar, ': ', trim(IOMSGvar), '; SEQUENTIAL: ', trim(SEQUENTIALvar)

  inquire (UNIT=5, SIGN=SIGNvar, IOSTAT=IOSTATvar, IOMSG=IOMSGvar)
  print *, IOSTATvar, ': ', trim(IOMSGvar), '; SIGN: ', trim(SIGNvar)

  inquire (UNIT=5, SIZE=SIZEvar, IOSTAT=IOSTATvar, IOMSG=IOMSGvar)
  print *, IOSTATvar, ': ', trim(IOMSGvar), '; SIZE: ', SIZEvar

  inquire (UNIT=5, STREAM=STREAMvar, IOSTAT=IOSTATvar, IOMSG=IOMSGvar)
  print *, IOSTATvar, ': ', trim(IOMSGvar), '; STREAM: ', trim(STREAMvar)

  inquire (UNIT=5, UNFORMATTED=UNFORMATTEDvar, IOSTAT=IOSTATvar, IOMSG=IOMSGvar)
  print *, IOSTATvar, ': ', trim(IOMSGvar), '; UNFORMATTED: ', trim(UNFORMATTEDvar)

  inquire (UNIT=5, WRITE=WRITEvar, IOSTAT=IOSTATvar, IOMSG=IOMSGvar)
  print *, IOSTATvar, ': ', trim(IOMSGvar), '; WRITE: ', trim(WRITEvar)

  inquire (FILE='nosuchfile.fil', EXIST=EXISTvar, ERR=100, IOSTAT=IOSTATvar, IOMSG=IOMSGvar)
  print *, IOSTATvar, ': ', trim(IOMSGvar), '; EXIST: ', EXISTvar

  print *, "Done"
  stop

100 continue
  print *, "Err:", IOSTATvar, ': ', trim(IOMSGvar), '; EXIST: ', EXISTvar
  stop

end program d321297e
